---
title: "Reinforcement Learning for Open Games"
description: "How to tap into the power of reinforcement learning while specifying and executing open games in Haskell."
author: Georgios Karachalias, Noon van der Silk
tags: [haskell, python]
---

In this post we illustrate how we built ["Learning Games"][full-code],
an integration between the [`open-games-hs`][open-games-fork] framework and [rllib][rllib],
in order to gain access to the entire Python ecosystem and train agents for games written in Haskell.

`open-games-hs` is a Haskell library and DSL for defining,
operating, and analyzing certain kinds of game-theoretic games. It comes
with a rich type theory and implementation that allows for the
specification of agent strategies as Haskell functions.

For some games, encoding strategies in Haskell is fairly simple. But one could
imagine another way: what if we could _learn_ those strategies? Utilizing the
theories of reinforcement learning? Well, one idea might be to try and build
a series of algorithms in Haskell implementing these strategies; _or_, we
could try and connect the Learning Games ecosystem to the Python AI ecosystem,
and see if we can leverage both, at the same time!

In this post, we illustrate how we combined Learning Games and rllib to easily model games using Haskell while training agents with Python.

## Running example: The Prisoner's Dilemma

To keep things simple, we focus on a single game throughout the post, but, as we discuss at the end, the same approach we used for this game can be applied to a whole family of games.

Broadly speaking, the concept of a [_Prisoner's Dilemma_][prisoners-dilemma] (PD) refers to a well-known paradoxical situation in which ["rational"][rational-gt] agents are incentivized to not cooperate with each other, even if it might be in their best interest to do so.

### Game description

Two people (the potential "prisoners", or "agents"), Agatha and Bibi, are thought to have committed a crime.
They are picked up and each is held in a holding cell.

They have no way to communicate with each other, and each has the option to either betray the other, or stay silent.
The possible outcomes, **known by all parties**, are:

- If both Agatha and Bibi betray one another, each of them serves two years.
- If Agatha betrays Bibi but Bibi stays silent, Agatha serves nothing and Bibi serves three years.
- If Agatha stays silent but Bibi betrays Agatha, Agatha serves three years and Bibi serves nothing.
- If Agatha and Bibi both stay silent, each of them serves one year (on a lesser charge).

### Nash equilibrium vs. cooperative solution

This game has what is known as a [_Nash equilibrium_][nash-equilibrium]: assuming that each agent has chosen their strategy, no agent can increase their expected payoff (i.e. reduce time served) assuming that the other agent keeps their strategy unchanged.
The equilibrium for this game is mutual betrayal, which is the best option from a "rational", self-interested perspective: defecting always results in a better payoff than cooperating, regardless of the other player's choice.
However, despite the unfavorable individual incentive to betray each other, the collectively better result would be for both agents to stay silent, so that they both serve a shorter sentence.

### Iterated variant

A more interesting version of the game — known as the _iterated_ prisoner's dilemma — is that in which the game is played more than once in succession, and each agent remembers the previous moves of the other agent.
This is the variant of the game we experimented with, since it is amenable to reinforcement learning: agents can be trained so that they can play optimally against their opponent's strategy and even learn to trust and cooperate with each other.
The latter is possible because the neural network can be trained and learn from historical data.

## Prisoner's dilemma as an open game

First, we encode the players' actions using a Haskell datatype.
Using standard nomenclature, each agent can either `Cooperate` (i.e. stay silent), or `Defect` (i.e. betray):

```haskell
data Action = Cooperate | Defect
```

Next, we implement the payoff calculation.
As is common when modelling this game, we use non-negative payoffs (i.e. rewards), as opposed to costs used in the original description of the game.
The possible values of `(PayoffA, PayoffB)` are summarized as follows:

|                    | B stays silent | B betrays A |
| ------------------ | :------------: | :---------: |
| **A stays silent** |     (3, 3)     |   (0, 5)    |
| **A betrays B**    |     (5, 0)     |   (1, 1)    |

The function below implements this table, computing the payoff for agent `i`, given `i`'s and `j`'s actions.

```haskell
prisonersDilemmaMatrix :: Action -> Action -> Double
prisonersDilemmaMatrix Cooperate Cooperate = 3
prisonersDilemmaMatrix Cooperate Defect    = 0
prisonersDilemmaMatrix Defect    Cooperate = 5
prisonersDilemmaMatrix Defect    Defect    = 1
```

### Game specification

Learning Games comes with [a tutorial][learning-games-modelling-tutorial] illustrating the semantics of the Template Haskell DSL in terms of [string diagrams][string-diagrams] and shows how to specify games using it.
Our intention in this post is not to explain this library in detail; for details please refer to the [Learning Games modelling tutorial][learning-games-modelling-tutorial].
In this DSL, the prisoner's dilemma looks like this:

```haskell
prisonersDilemma = [opengame|
   inputs    :                                                        ;
   feedback  : (payoff0,payoff1)                                      ;
   :----------------------------:
   inputs    :                                                        ;
   feedback  : payoff0                                                ;
   operation : interactWithEnv                                        ;
   outputs   : decisionPlayer0                                        ;
   returns   : prisonersDilemmaMatrix decisionPlayer0 decisionPlayer1 ;

   inputs    :                                                        ;
   feedback  : payoff1                                                ;
   operation : interactWithEnv                                        ;
   outputs   : decisionPlayer1                                        ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer0 ;
   :----------------------------:
   outputs   : (decisionPlayer0, decisionPlayer1)                     ;
   returns   :                                                        ;
  |]
```

It describes the prisoner's dilemma as the composition of two standalone sub-games.
Interestingly, each subgame needs to know the decision of the opponent to calculate its payoff (using the `prisonersDilemmaMatrix` function).
We can represent this information flow graphically as follows:

<p float="left">
  <img src="./pd-diagram.svg" width="49%" />
</p>

Given a `strategy` of type `List '[Action, Action]` (i.e. a [heterogeneous list][heterogeneous-lists] containing the decisions of the first and the second agent), we can use the framework to run the game once and observe the resulting payoffs.

```haskell
extractPayoffAndNextState (play prisonersDilemma strategy) () ()
  :: IO ((Double, Double), (Action, Action)) -- (feedback, output)
```

Note that, given we want to play the games step-by-step, we need to break games up so that, if they consist of several rounds, they output their internal state, and that state is then passed into the next game.
This is a technical limitation that we aim to clean up in subsequent work.

## The game server

A general challenge in utilizing rllib for playing open games is figuring out
how to make the two frameworks speak to each other, given that they are
written in different languages. Our strategy for addressing this issue is the
following:

- Turn the open games side into [a server][servant-server] and the rllib side
  into a client.
- Have the server and the client exchange information (agent strategies,
  payoffs, etc.) encoded as [JSON objects][aeson].

To account for the possibility of rllib utilizing parallelism during
training, as well as getting a performance edge, we chose to use
[websockets][servant-websockets] instead of plain http. This way we avoid
managing game sessions and reduce communication overhead between the server
and the client.

Most of the server infrastructure is standard for servant-based applications,
and at its core sits the following function:

```haskell
wsPlay :: PendingConnection -> Handler ()
wsPlay pending = do
  liftIO $ do
    connection <- WS.acceptRequest pending
    handle (const (pure ())) . WS.withPingThread connection 10 (pure ()) $ liftIO $ forever $ do
      -- (a) Read some game actions from the websocket
      Just (PlayParameters { player0Action, player1Action }) <- decode <$> WS.receiveData @ByteString connection

      -- (b) Play one step of the game with these actions, and obtain the payoffs.
      let strategy  = player0Action ::- player1Action ::- Nil
      (payoff0, payoff1) <- fst <$> extractPayoffAndNextState (play prisonersDilemma strategy) () ()
```

`wsPlay` takes a pending connection to a websocket and then indefinitely:

1. receives and decodes JSON objects containing a pair of actions (for agents 0 and 1, respectively)
2. uses these actions to execute one round of prisoner's dilemma and observe the corresponding payoffs
3. encodes said payoffs as another JSON object, which it then sends back to the websocket.

Once the server is up and running, one can send it messages directly, e.g. via [`websocat`][websocat] like so:

```bash
$ echo '{"player0Action": "Defect", "player1Action": "Defect"}' | websocat ws://localhost:3000/prisoners-dilemma/play
{"player0Payoff":1,"player1Payoff":1}
```

Or we can do so from a reinforcement learning client — exciting times!

## The client: Reinforcement learning with rllib

With the server side out of the way, let's focus now on the client side.

Let's briefly remind ourselves of the gist of reinforcement learning.
The general idea is that we establish an environment, we make moves in that environment, and the environment tells us how good each move was.
We can can use rllib to codify this structure, supposing we can come up with a way to map our problem onto this model.
Of course, it so happens we can:
the two players serve as our "agents" and the Haskell Learning Games library as the "environment"; all is left to do is define valid actions!

We can use rllib's [tune][tune-run] entrypoint for training agents, which essentially requires two things:

1. a specification of the game _environment_, including _observations_[^1] and the _action space_[^2] that comes in the form of a user-defined subclass of rllib's `MultiAgentEnv` class, and
2. a game-specific configuration dictionary, specifying general information needed by rllib, such as the number of GPUs to be used, and game-specific information, such as agent strategies; this configuration is also used when creating objects of the custom class.

[^1]:
    The observations, or observation space, is the set of data that the RL training code will look at, in order to decide how to act.
    In typical RL, this might be the literal pixels of the game, say from an Atari, or perhaps the state of the board when playing Go.
    In our case, it mostly consisted of the moves of the "other" player.

[^2]:
    An "action space" refers to the set of valid actions that the agent can make, at any time.
    In some games, this is very simple; i.e. in the "Rock-Paper-Scissors" game, it is simply `[Rock, Paper, Scissors]`.
    In some games, certain moves are only valid at certain times.
    There are a few ways of addressing this, perhaps the easiest of which is simply penalizing invalid moves via a negative reward.

For every open game we experimented with we manually defined both items.
Let's take a look at their most interesting aspects.

### Hard-coding agent strategies

Perhaps the most interesting part of the configuration dictionary for us is the `multiagent` field, which allows for choosing different policies for different agents in multiagent games (e.g. always defecting, copying the opponent's move, etc).
In our implementation we populate this field by means of the following function:

```python
def make_multiagent_config(policy0, policy1):
    return {
        "policies_to_train": ["player_0", ...],
        "policies": {
            "player_0": policy0.policy,
            "player_1": policy1.policy,
            }
        }
```

There are a few details left out, but basically it lets us allocate a particular strategy (either "learned", or some particular hard-coded policy) to a particular player.

Having an RL framework that supports multiagent training was one of our primary reasons to pick rllib in the first place; and this seems like a particularly neat approach.

This is also an especially important feature for analyzing whether or not the training side is learning "correctly"; we can simply set some hard-coded strategy for one of the agents, allow the other one to learn, and check to see if it does the "best thing" given that strategy.

### Subclassing `MultiAgentEnv`

The way we found to build a multiagent environment was to create a subclass of [MultiAgentEnv][multiagentenv] and override three of its methods.

#### 1. Initialization

During [initialization][code-init] the game is set up and essential game information is stored into the game object, such as the action space and the observation space.

For PD in particular, the action space is [discrete][gym-discrete] and contains only two moves (`0` for cooperating and `1` for defecting).
Our observations are a pair of the moves that each player chose to make.

Note that the particular representation that the action space and the observation space take on depends heavily on the learning algorithm that you will use them with.
We are using the standard [Policy Gradient][pg] implementation, which supports our selection of spaces.

#### 2. Resetting

A _step_ is one move (or one round) of the game.
All steps of the game make up an _episode_.

During training, [the game must be reset][code-reset] between different episodes.
When resetting, we also close and re-open the websocket connection.

Some games might end early (e.g. in Mario, you may be eaten by a flower), while some games always have a single round (e.g. Rock-Paper-Scissors, or the Prisoner's Dilemma).
Making those games repeat in the same episode instead of many episodes turns out to work better with rllib.
Still, episodes are limited by a fixed _episode length_.

#### 3. Taking a step

The [most crucial method is `step`][code-step] which, given a dictionary `action_dict` containing the action of each player, executes one game step.

First, it creates a JSON object containing the two moves and sends it to the server via the active socket:

```python
class DiscreteTwoPlayerLearningGamesEnv(MultiAgentEnv):
    ...

    def step(self, action_dict):
        ...

        # Build the data
        data = {
            f"player{i}Action": self.action_map[action_dict[i]]
            for i in range(self.num_agents)
        }

        # Send the actions on the socket
        self.ws.send(json.dumps(data))

        ...
```

then it reads back the corresponding payoffs from the socket:

```python
        # Receive the payoffs message
        response = json.loads(self.ws.recv())
```

Last, it computes rewards and observations on a per-agent basis that are to be fed to the learning algorithm, and checks whether the game has come to an end:

```python
        rewards      = { i: response[f"player{i}Payoff"] for i in range(self.num_agents) }
        observations = { i: (action_dict[0], action_dict[1]) for i in range(self.num_agents) }
        dones        = { i: is_done for i in range(self.num_agents) }
```

where `is_done` is simply a check that we've done enough steps:

```python
is_done = step_number >= episode_length
```

There are a few more technical details, which you can see in [our full implementation][full-code], but most of it is fairly straightforward.

On with the training!

## Training against different strategies

With all the setup out of the way, now it's time to have some fun!
The most exciting part of this project was experimenting with different agent policies in rllib and watching the learning agents learn how to play optimally against the opponent strategies _in real time_, on [tensorboard][tensorboard].

With all our infrastructure in place, this became a pretty straightforward task:

```python
ray.init()

tune.run(
    "PG",
    config = make_pd_config(learned, always_defect),
    stop   = {"timesteps_total": 25_000},
    ...
)

tune.run(
    "PG",
    config = make_pd_config(random_pd_move, learned),
    stop   = { "timesteps_total": 25_000 },
    ...
)
...
```

### Against an agent that always defects

When `player1` consistently defects, the optimal strategy for `player0` is to always defect as well, for an average payoff of 1.
If `player0` were to cooperate, their payoff would be 0.
This is observed in the payoff curves, where `player0`'s payoff converges to 1 starting from under one (minimum: 0), and `player1`'s payoff converges to 1 starting from over 1 (maximum: 5).

<p float="left">
  <img src="./tensorboard-graphs/pd_learned_vs_always_defect_ep_length_10_ray_tune_sampler_results_custom_metrics_player_0_step_average_mean.svg" width="49%" />
  <img src="./tensorboard-graphs/pd_learned_vs_always_defect_ep_length_10_ray_tune_sampler_results_custom_metrics_player_1_step_average_mean.svg" width="49%" />
</p>

### Against random moves

When `player1` plays random moves, the optimal strategy for `player0` is to always defect.
Since — statistically speaking — 50% of the time `player1` will be defecting and 50% of the time they will be cooperating, `player0` makes a payoff of 1 half of the time (when `player1` happens to defect as well) and a payoff of 5 the other half of the time (when `player1` happens to cooperate), which gives an average payoff of 3, as observed in the graph for `player0`.
Similarly, the payoff of `player1` converges towards 0.5; half of the time it is 0 and the other half it is 1.

<p float="left">
  <img src="./tensorboard-graphs/pd_learned_vs_random_ep_length_1000_ray_tune_sampler_results_custom_metrics_player_0_step_average_mean.svg" width="49%" />
  <img src="./tensorboard-graphs/pd_learned_vs_random_ep_length_1000_ray_tune_sampler_results_custom_metrics_player_1_step_average_mean.svg" width="49%" />
</p>

### Against tit-for-tat

When `player1` uses tit-for-tat, the optimal strategy for `player0` is to do the same, so that both players perpetually cooperate, for a mutually beneficial result where both players have a payoff of 3.

<p float="left">
  <img src="./tensorboard-graphs/pd_learned_vs_tit_for_tat_ep_len_10_ray_tune_sampler_results_custom_metrics_player_0_step_average_mean.svg" width="49%" />
  <img src="./tensorboard-graphs/pd_learned_vs_tit_for_tat_ep_len_10_ray_tune_sampler_results_custom_metrics_player_1_step_average_mean.svg" width="49%" />
</p>

### Against another learning agent

When both agents are learners and the length of an episode (i.e. the number of past rounds the agents have memory of) is low, both players are incentivized to defect for a payoff of 1 per agent.
This is not necessarily optimal but, as we mentioned earlier, is a Nash equilibrium for prisoner's dilemma.
However, by setting the episode length sufficiently high (100 rounds), we finally observed both agents learning to cooperate for a payoff of 3 per agent.

<p float="left">
  <img src="./tensorboard-graphs/pd_learned_vs_learned_ep_length_100_ray_tune_sampler_results_custom_metrics_player_0_step_average_mean.svg" width="49%" />
  <img src="./tensorboard-graphs/pd_learned_vs_learned_ep_length_100_ray_tune_sampler_results_custom_metrics_player_1_step_average_mean.svg" width="49%" />
</p>

## Conclusion

All in all, this experiment has shown very promising results: daunting though the task initially looked, connecting the two libraries was successful.

We tried three other games requiring a different setup, as it could be easily changed:

|                                                       | Stateful | Steps | Players |
| ----------------------------------------------------- | :------: | :---: | :-----: |
| [(iterated) Prisoner's Dilemma][prisoners-dilemma]    |    No    |   1   |    2    |
| [(iterated) Rock-Paper-Scissors][rock-paper-scissors] |    No    |   1   |    2    |
| [Monty Hall][monty-hall]                              |   Yes    |   2   |    1    |
| [Trust Game][trust-game]                              |   Yes    |   2   |    2    |

For the multi-step games in this list (Monty Hall, Trust Game) we decomposed the monolithic game definition into sub-games.
This was necessary for interleaving game execution and communication with the client between each step.
We performed this transformation manually this time, but we believe that this process can (and should, in the future) be automated by the Haskell DSL for a large class of games — after all, modularity is one of the greatest strengths of Learning Games.

Another question that we have not addressed in this blog post is whether learned agents can perform optimally against strategies they have not been trained against.
Can an agent that knows how to defeat an ever-defecting opponent also defeat an ever-cooperating one?

[aeson]: https://hackage.haskell.org/package/aeson
[code-init]: https://github.com/Learning-Games/open-games-RLib/blob/68fb06636430a68d174a32983c0a7403148ff656/rllib-client/ma/env.py#L12
[code-reset]: https://github.com/Learning-Games/open-games-RLib/blob/68fb06636430a68d174a32983c0a7403148ff656/rllib-client/ma/env.py#L67
[code-step]: https://github.com/Learning-Games/open-games-RLib/blob/68fb06636430a68d174a32983c0a7403148ff656/rllib-client/ma/env.py#L83
[full-code]: https://github.com/Learning-Games/open-games-RLib
[gym-discrete]: https://github.com/openai/gym/blob/master/gym/spaces/discrete.py#L10
[heterogeneous-lists]: https://github.com/Learning-Games/open-games-RLib/blob/68fb06636430a68d174a32983c0a7403148ff656/src/Engine/TLL.hs
[learning-games-modelling-tutorial]: https://github.com/philipp-zahn/open-games-hs/blob/0de997aca1de94003842365173aa530fb0c3fd4d/Tutorial/TUTORIAL.md
[monty-hall]: https://en.wikipedia.org/wiki/Monty_Hall_problem
[multiagentenv]: https://docs.ray.io/en/latest/rllib/package_ref/env/multi_agent_env.html
[nash-equilibrium]: https://en.wikipedia.org/wiki/Nash_equilibrium
[open-games-fork]: https://github.com/philipp-zahn/open-games-hs
[pg]: https://docs.ray.io/en/latest/rllib/rllib-algorithms.html#pg
[prisoners-dilemma]: https://en.wikipedia.org/wiki/Prisoner%27s_dilemma
[rational-gt]: https://en.wikipedia.org/wiki/Rational_agent
[rllib]: https://docs.ray.io/en/latest/rllib/index.html
[rock-paper-scissors]: https://en.wikipedia.org/wiki/Rock_paper_scissors
[servant-server]: https://hackage.haskell.org/package/servant-server
[servant-websockets]: https://hackage.haskell.org/package/servant-websockets
[string-diagrams]: https://en.wikipedia.org/wiki/String_diagram
[tensorboard]: https://www.tensorflow.org/tensorboard
[trust-game]: https://en.wikibooks.org/wiki/Bestiary_of_Behavioral_Economics/Trust_Game
[tune-run]: https://github.com/ray-project/ray/blob/bcf77f38ee60f493c8c44de25e14f187856afead/python/ray/tune/tune.py#L126
[websocat]: https://github.com/vi/websocat
[websockets]: https://hackage.haskell.org/package/websockets
