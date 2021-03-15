// in view port init
function initInViewport() {
  jQuery(".viewport-section").itemInViewport({
    visibleMode: 2,
    once: true,
  })
}

// mobile menu init
function initMobileNav() {
  jQuery("body").mobileNav({
    menuActiveClass: "menu-active",
    menuOpener: ".menu-opener",
    hideOnClickOutside: true,
    menuDrop: ".nav-drop",
  })
}

/*
 * jQuery sticky box plugin
 */
;(function ($, $win) {
  "use strict"

  function StickyScrollBlock($stickyBox, options) {
    this.options = options
    this.$stickyBox = $stickyBox
    this.init()
  }

  var StickyScrollBlockPrototype = {
    init: function () {
      this.findElements()
      this.attachEvents()
      this.makeCallback("onInit")
    },

    findElements: function () {
      // find parent container in which will be box move
      this.$container = this.$stickyBox.closest(this.options.container)
      // define box wrap flag
      this.isWrap =
        this.options.positionType === "fixed" && this.options.setBoxHeight
      // define box move flag
      this.moveInContainer = !!this.$container.length
      // wrapping box to set place in content
      if (this.isWrap) {
        this.$stickyBoxWrap = this.$stickyBox
          .wrap('<div class="' + this.getWrapClass() + '"/>')
          .parent()
      }
      //define block to add active class
      this.parentForActive = this.getParentForActive()
      this.isInit = true
    },

    attachEvents: function () {
      var self = this

      // bind events
      this.onResize = function () {
        if (!self.isInit) return
        self.resetState()
        self.recalculateOffsets()
        self.checkStickyPermission()
        self.scrollHandler()
      }

      this.onScroll = function () {
        self.scrollHandler()
      }

      // initial handler call
      this.onResize()

      // handle events
      $win
        .on("load resize orientationchange", this.onResize)
        .on("scroll", this.onScroll)
    },

    defineExtraTop: function () {
      // define box's extra top dimension
      var extraTop

      if (typeof this.options.extraTop === "number") {
        extraTop = this.options.extraTop
      } else if (typeof this.options.extraTop === "function") {
        extraTop = this.options.extraTop()
      }

      this.extraTop =
        this.options.positionType === "absolute"
          ? extraTop
          : Math.min(this.winParams.height - this.data.boxFullHeight, extraTop)
    },

    checkStickyPermission: function () {
      // check the permission to set sticky
      this.isStickyEnabled = this.moveInContainer
        ? this.data.containerOffsetTop + this.data.containerHeight >
          this.data.boxFullHeight +
            this.data.boxOffsetTop +
            this.options.extraBottom
        : true
    },

    getParentForActive: function () {
      if (this.isWrap) {
        return this.$stickyBoxWrap
      }

      if (this.$container.length) {
        return this.$container
      }

      return this.$stickyBox
    },

    getWrapClass: function () {
      // get set of container classes
      try {
        return this.$stickyBox
          .attr("class")
          .split(" ")
          .map(function (name) {
            return "sticky-wrap-" + name
          })
          .join(" ")
      } catch (err) {
        return "sticky-wrap"
      }
    },

    resetState: function () {
      // reset dimensions and state
      this.stickyFlag = false
      this.$stickyBox
        .css({
          "-webkit-transition": "",
          "-webkit-transform": "",
          transition: "",
          transform: "",
          position: "",
          width: "",
          left: "",
          top: "",
        })
        .removeClass(this.options.activeClass)

      if (this.isWrap) {
        this.$stickyBoxWrap
          .removeClass(this.options.activeClass)
          .removeAttr("style")
      }

      if (this.moveInContainer) {
        this.$container.removeClass(this.options.activeClass)
      }
    },

    recalculateOffsets: function () {
      // define box and container dimensions
      this.winParams = this.getWindowParams()

      this.data = $.extend(this.getBoxOffsets(), this.getContainerOffsets())

      this.defineExtraTop()
    },

    getBoxOffsets: function () {
      function offetTop(obj) {
        obj.top = 0
        return obj
      }
      var boxOffset =
        this.$stickyBox.css("position") === "fixed"
          ? offetTop(this.$stickyBox.offset())
          : this.$stickyBox.offset()
      var boxPosition = this.$stickyBox.position()

      return {
        // sticky box offsets
        boxOffsetLeft: boxOffset.left,
        boxOffsetTop: boxOffset.top,
        // sticky box positions
        boxTopPosition: boxPosition.top,
        boxLeftPosition: boxPosition.left,
        // sticky box width/height
        boxFullHeight: this.$stickyBox.outerHeight(true),
        boxHeight: this.$stickyBox.outerHeight(),
        boxWidth: this.$stickyBox.outerWidth(),
      }
    },

    getContainerOffsets: function () {
      var containerOffset = this.moveInContainer
        ? this.$container.offset()
        : null

      return containerOffset
        ? {
            // container offsets
            containerOffsetLeft: containerOffset.left,
            containerOffsetTop: containerOffset.top,
            // container height
            containerHeight: this.$container.outerHeight(),
          }
        : {}
    },

    getWindowParams: function () {
      return {
        height: window.innerHeight || document.documentElement.clientHeight,
      }
    },

    makeCallback: function (name) {
      if (typeof this.options[name] === "function") {
        var args = Array.prototype.slice.call(arguments)
        args.shift()
        this.options[name].apply(this, args)
      }
    },

    destroy: function () {
      this.isInit = false
      // remove event handlers and styles
      $win
        .off("load resize orientationchange", this.onResize)
        .off("scroll", this.onScroll)
      this.resetState()
      this.$stickyBox.removeData("StickyScrollBlock")
      if (this.isWrap) {
        this.$stickyBox.unwrap()
      }
      this.makeCallback("onDestroy")
    },
  }

  var stickyMethods = {
    fixed: {
      scrollHandler: function () {
        this.winScrollTop = $win.scrollTop()
        var isActiveSticky =
          this.winScrollTop -
            (this.options.showAfterScrolled ? this.extraTop : 0) -
            (this.options.showAfterScrolled
              ? this.data.boxHeight + this.extraTop
              : 0) >
          this.data.boxOffsetTop - this.extraTop

        if (isActiveSticky) {
          this.isStickyEnabled && this.stickyOn()
        } else {
          this.stickyOff()
        }
      },

      stickyOn: function () {
        if (!this.stickyFlag) {
          this.stickyFlag = true
          this.parentForActive.addClass(this.options.activeClass)
          this.$stickyBox.css({
            width: this.data.boxWidth,
            position: this.options.positionType,
          })
          if (this.isWrap) {
            this.$stickyBoxWrap.css({
              height: this.data.boxFullHeight,
            })
          }
          this.makeCallback("fixedOn")
        }
        this.setDynamicPosition()
      },

      stickyOff: function () {
        if (this.stickyFlag) {
          this.stickyFlag = false
          this.resetState()
          this.makeCallback("fixedOff")
        }
      },

      setDynamicPosition: function () {
        this.$stickyBox.css({
          top: this.getTopPosition(),
          left: this.data.boxOffsetLeft - $win.scrollLeft(),
        })
      },

      getTopPosition: function () {
        if (this.moveInContainer) {
          var currScrollTop =
            this.winScrollTop + this.data.boxHeight + this.options.extraBottom

          return Math.min(
            this.extraTop,
            this.data.containerHeight +
              this.data.containerOffsetTop -
              currScrollTop
          )
        } else {
          return this.extraTop
        }
      },
    },
    absolute: {
      scrollHandler: function () {
        this.winScrollTop = $win.scrollTop()
        var isActiveSticky =
          this.winScrollTop > this.data.boxOffsetTop - this.extraTop

        if (isActiveSticky) {
          this.isStickyEnabled && this.stickyOn()
        } else {
          this.stickyOff()
        }
      },

      stickyOn: function () {
        if (!this.stickyFlag) {
          this.stickyFlag = true
          this.parentForActive.addClass(this.options.activeClass)
          this.$stickyBox.css({
            width: this.data.boxWidth,
            transition: "transform " + this.options.animSpeed + "s ease",
            "-webkit-transition":
              "transform " + this.options.animSpeed + "s ease",
          })

          if (this.isWrap) {
            this.$stickyBoxWrap.css({
              height: this.data.boxFullHeight,
            })
          }

          this.makeCallback("fixedOn")
        }

        this.clearTimer()
        this.timer = setTimeout(
          function () {
            this.setDynamicPosition()
          }.bind(this),
          this.options.animDelay * 1000
        )
      },

      stickyOff: function () {
        if (this.stickyFlag) {
          this.clearTimer()
          this.stickyFlag = false

          this.timer = setTimeout(
            function () {
              this.setDynamicPosition()
              setTimeout(
                function () {
                  this.resetState()
                }.bind(this),
                this.options.animSpeed * 1000
              )
            }.bind(this),
            this.options.animDelay * 1000
          )
          this.makeCallback("fixedOff")
        }
      },

      clearTimer: function () {
        clearTimeout(this.timer)
      },

      setDynamicPosition: function () {
        var topPosition = Math.max(0, this.getTopPosition())

        this.$stickyBox.css({
          transform: "translateY(" + topPosition + "px)",
          "-webkit-transform": "translateY(" + topPosition + "px)",
        })
      },

      getTopPosition: function () {
        var currTopPosition =
          this.winScrollTop - this.data.boxOffsetTop + this.extraTop

        if (this.moveInContainer) {
          var currScrollTop =
            this.winScrollTop + this.data.boxHeight + this.options.extraBottom
          var diffOffset = Math.abs(
            Math.min(
              0,
              this.data.containerHeight +
                this.data.containerOffsetTop -
                currScrollTop -
                this.extraTop
            )
          )

          return currTopPosition - diffOffset
        } else {
          return currTopPosition
        }
      },
    },
  }

  // jQuery plugin interface
  $.fn.stickyScrollBlock = function (opt) {
    var args = Array.prototype.slice.call(arguments)
    var method = args[0]

    var options = $.extend(
      {
        container: null,
        positionType: "fixed", // 'fixed' or 'absolute'
        activeClass: "fixed-position",
        setBoxHeight: true,
        showAfterScrolled: false,
        extraTop: 0,
        extraBottom: 0,
        animDelay: 0.1,
        animSpeed: 0.2,
      },
      opt
    )

    return this.each(function () {
      var $stickyBox = jQuery(this)
      var instance = $stickyBox.data("StickyScrollBlock")

      if (typeof opt === "object" || typeof opt === "undefined") {
        StickyScrollBlock.prototype = $.extend(
          stickyMethods[options.positionType],
          StickyScrollBlockPrototype
        )
        $stickyBox.data(
          "StickyScrollBlock",
          new StickyScrollBlock($stickyBox, options)
        )
      } else if (typeof method === "string" && instance) {
        if (typeof instance[method] === "function") {
          args.shift()
          instance[method].apply(instance, args)
        }
      }
    })
  }

  // module exports
  window.StickyScrollBlock = StickyScrollBlock
})(jQuery, jQuery(window))

/*
 * jQuery In Viewport plugin
 */
;(function ($, $win) {
  "use strict"

  var ScrollDetector = (function () {
    var data = {}

    return {
      init: function () {
        var self = this

        this.addHolder("win", $win)

        $win.on(
          "load.blockInViewport resize.blockInViewport orientationchange.blockInViewport",
          function () {
            $.each(data, function (holderKey, holderData) {
              self.calcHolderSize(holderData)

              $.each(holderData.items, function (itemKey, itemData) {
                self.calcItemSize(itemKey, itemData)
              })
            })
          }
        )
      },

      addHolder: function (holderKey, $holder) {
        var self = this
        var holderData = {
          holder: $holder,
          items: {},
          props: {
            height: 0,
            scroll: 0,
          },
        }

        data[holderKey] = holderData

        $holder.on("scroll.blockInViewport", function () {
          self.calcHolderScroll(holderData)

          $.each(holderData.items, function (itemKey, itemData) {
            self.calcItemScroll(itemKey, itemData)
          })
        })

        this.calcHolderSize(data[holderKey])
      },

      calcHolderSize: function (holderData) {
        var holderOffset =
          window.self !== holderData.holder[0] ? holderData.holder.offset() : 0

        holderData.props.height =
          holderData.holder.get(0) === window
            ? window.innerHeight || document.documentElement.clientHeight
            : holderData.holder.outerHeight()
        holderData.props.offset = holderOffset ? holderOffset.top : 0

        this.calcHolderScroll(holderData)
      },

      calcItemSize: function (itemKey, itemData) {
        itemData.offset =
          itemData.$el.offset().top - itemData.holderProps.props.offset
        itemData.height = itemData.$el.outerHeight()

        this.calcItemScroll(itemKey, itemData)
      },

      calcHolderScroll: function (holderData) {
        holderData.props.scroll = holderData.holder.scrollTop()
      },

      calcItemScroll: function (itemKey, itemData) {
        var itemInViewPortFromUp
        var itemInViewPortFromDown
        var itemOutViewPort
        var holderProps = itemData.holderProps.props

        switch (itemData.options.visibleMode) {
          case 1:
            itemInViewPortFromDown =
              itemData.offset < holderProps.scroll + holderProps.height / 2 ||
              itemData.offset + itemData.height <
                holderProps.scroll + holderProps.height
            itemInViewPortFromUp =
              itemData.offset > holderProps.scroll ||
              itemData.offset + itemData.height >
                holderProps.scroll + holderProps.height / 2
            break

          case 2:
            itemInViewPortFromDown =
              itemInViewPortFromDown ||
              itemData.offset < holderProps.scroll + holderProps.height / 2 ||
              itemData.offset + itemData.height / 2 <
                holderProps.scroll + holderProps.height
            itemInViewPortFromUp =
              itemInViewPortFromUp ||
              itemData.offset + itemData.height / 2 > holderProps.scroll ||
              itemData.offset + itemData.height >
                holderProps.scroll + holderProps.height / 2
            break

          case 3:
            itemInViewPortFromDown =
              itemInViewPortFromDown ||
              itemData.offset < holderProps.scroll + holderProps.height / 2 ||
              itemData.offset < holderProps.scroll + holderProps.height
            itemInViewPortFromUp =
              itemInViewPortFromUp ||
              itemData.offset + itemData.height > holderProps.scroll ||
              itemData.offset + itemData.height >
                holderProps.scroll + holderProps.height / 2
            break

          default:
            itemInViewPortFromDown =
              itemInViewPortFromDown ||
              itemData.offset < holderProps.scroll + holderProps.height / 2 ||
              itemData.offset +
                Math.min(itemData.options.visibleMode, itemData.height) <
                holderProps.scroll + holderProps.height
            itemInViewPortFromUp =
              itemInViewPortFromUp ||
              itemData.offset +
                itemData.height -
                Math.min(itemData.options.visibleMode, itemData.height) >
                holderProps.scroll ||
              itemData.offset + itemData.height >
                holderProps.scroll + holderProps.height / 2
            break
        }

        if (itemInViewPortFromUp && itemInViewPortFromDown) {
          if (!itemData.state) {
            itemData.state = true
            itemData.$el
              .addClass(itemData.options.activeClass)
              .trigger("in-viewport", true)

            if (
              itemData.options.once ||
              ($.isFunction(itemData.options.onShow) &&
                itemData.options.onShow(itemData))
            ) {
              delete itemData.holderProps.items[itemKey]
            }
          }
        } else {
          itemOutViewPort =
            itemData.offset < holderProps.scroll + holderProps.height &&
            itemData.offset + itemData.height > holderProps.scroll

          if ((itemData.state || isNaN(itemData.state)) && !itemOutViewPort) {
            itemData.state = false
            itemData.$el
              .removeClass(itemData.options.activeClass)
              .trigger("in-viewport", false)
          }
        }
      },

      addItem: function (el, options) {
        var itemKey = "item" + this.getRandomValue()
        var newItem = {
          $el: $(el),
          options: options,
        }
        var holderKeyDataName = "in-viewport-holder"

        var $holder = newItem.$el.closest(options.holder)
        var holderKey = $holder.data(holderKeyDataName)

        if (!$holder.length) {
          holderKey = "win"
        } else if (!holderKey) {
          holderKey = "holder" + this.getRandomValue()
          $holder.data(holderKeyDataName, holderKey)

          this.addHolder(holderKey, $holder)
        }

        newItem.holderProps = data[holderKey]

        data[holderKey].items[itemKey] = newItem

        this.calcItemSize(itemKey, newItem)
      },

      getRandomValue: function () {
        return (Math.random() * 100000).toFixed(0)
      },

      destroy: function () {
        $win.off(".blockInViewport")

        $.each(data, function (key, value) {
          value.holder.off(".blockInViewport")

          $.each(value.items, function (key, value) {
            value.$el.removeClass(value.options.activeClass)
            value.$el.get(0).itemInViewportAdded = null
          })
        })

        data = {}
      },
    }
  })()

  ScrollDetector.init()

  $.fn.itemInViewport = function (options) {
    options = $.extend(
      {
        activeClass: "in-viewport",
        once: true,
        holder: "",
        visibleMode: 1, // 1 - full block, 2 - half block, 3 - immediate, 4... - custom
      },
      options
    )

    return this.each(function () {
      if (this.itemInViewportAdded) {
        return
      }

      this.itemInViewportAdded = true

      ScrollDetector.addItem(this, options)
    })
  }
})(jQuery, jQuery(window))

/*
 * Responsive Layout helper
 */
window.ResponsiveHelper = (function ($) {
  // init variables
  var handlers = [],
    prevWinWidth,
    win = $(window),
    nativeMatchMedia = false

  // detect match media support
  if (window.matchMedia) {
    if (window.Window && window.matchMedia === Window.prototype.matchMedia) {
      nativeMatchMedia = true
    } else if (window.matchMedia.toString().indexOf("native") > -1) {
      nativeMatchMedia = true
    }
  }

  // prepare resize handler
  function resizeHandler() {
    var winWidth = win.width()
    if (winWidth !== prevWinWidth) {
      prevWinWidth = winWidth

      // loop through range groups
      $.each(handlers, function (index, rangeObject) {
        // disable current active area if needed
        $.each(rangeObject.data, function (property, item) {
          if (item.currentActive && !matchRange(item.range[0], item.range[1])) {
            item.currentActive = false
            if (typeof item.disableCallback === "function") {
              item.disableCallback()
            }
          }
        })

        // enable areas that match current width
        $.each(rangeObject.data, function (property, item) {
          if (!item.currentActive && matchRange(item.range[0], item.range[1])) {
            // make callback
            item.currentActive = true
            if (typeof item.enableCallback === "function") {
              item.enableCallback()
            }
          }
        })
      })
    }
  }
  win.bind("load resize orientationchange", resizeHandler)

  // test range
  function matchRange(r1, r2) {
    var mediaQueryString = ""
    if (r1 > 0) {
      mediaQueryString += "(min-width: " + r1 + "px)"
    }
    if (r2 < Infinity) {
      mediaQueryString +=
        (mediaQueryString ? " and " : "") + "(max-width: " + r2 + "px)"
    }
    return matchQuery(mediaQueryString, r1, r2)
  }

  // media query function
  function matchQuery(query, r1, r2) {
    if (window.matchMedia && nativeMatchMedia) {
      return matchMedia(query).matches
    } else if (window.styleMedia) {
      return styleMedia.matchMedium(query)
    } else if (window.media) {
      return media.matchMedium(query)
    } else {
      return prevWinWidth >= r1 && prevWinWidth <= r2
    }
  }

  // range parser
  function parseRange(rangeStr) {
    var rangeData = rangeStr.split("..")
    var x1 = parseInt(rangeData[0], 10) || -Infinity
    var x2 = parseInt(rangeData[1], 10) || Infinity
    return [x1, x2].sort(function (a, b) {
      return a - b
    })
  }

  // export public functions
  return {
    addRange: function (ranges) {
      // parse data and add items to collection
      var result = { data: {} }
      $.each(ranges, function (property, data) {
        result.data[property] = {
          range: parseRange(property),
          enableCallback: data.on,
          disableCallback: data.off,
        }
      })
      handlers.push(result)

      // call resizeHandler to recalculate all events
      prevWinWidth = null
      resizeHandler()
    },
  }
})(jQuery)

/*
 * Simple Mobile Navigation
 */
;(function ($) {
  function MobileNav(options) {
    this.options = $.extend(
      {
        container: null,
        hideOnClickOutside: false,
        menuActiveClass: "nav-active",
        menuOpener: ".nav-opener",
        menuDrop: ".nav-drop",
        toggleEvent: "click",
        outsideClickEvent: "click touchstart pointerdown MSPointerDown",
      },
      options
    )
    this.initStructure()
    this.attachEvents()
  }
  MobileNav.prototype = {
    initStructure: function () {
      this.page = $("html")
      this.container = $(this.options.container)
      this.opener = this.container.find(this.options.menuOpener)
      this.drop = this.container.find(this.options.menuDrop)
    },
    attachEvents: function () {
      var self = this

      if (activateResizeHandler) {
        activateResizeHandler()
        activateResizeHandler = null
      }

      this.outsideClickHandler = function (e) {
        if (self.isOpened()) {
          var target = $(e.target)
          if (
            !target.closest(self.opener).length &&
            !target.closest(self.drop).length
          ) {
            self.hide()
          }
        }
      }

      this.openerClickHandler = function (e) {
        e.preventDefault()
        self.toggle()
      }

      this.opener.on(this.options.toggleEvent, this.openerClickHandler)
    },
    isOpened: function () {
      return this.container.hasClass(this.options.menuActiveClass)
    },
    show: function () {
      this.container.addClass(this.options.menuActiveClass)
      if (this.options.hideOnClickOutside) {
        this.page.on(this.options.outsideClickEvent, this.outsideClickHandler)
      }
    },
    hide: function () {
      this.container.removeClass(this.options.menuActiveClass)
      if (this.options.hideOnClickOutside) {
        this.page.off(this.options.outsideClickEvent, this.outsideClickHandler)
      }
    },
    toggle: function () {
      if (this.isOpened()) {
        this.hide()
      } else {
        this.show()
      }
    },
    destroy: function () {
      this.container.removeClass(this.options.menuActiveClass)
      this.opener.off(this.options.toggleEvent, this.clickHandler)
      this.page.off(this.options.outsideClickEvent, this.outsideClickHandler)
    },
  }

  var activateResizeHandler = function () {
    var win = $(window),
      doc = $("html"),
      resizeClass = "resize-active",
      flag,
      timer
    var removeClassHandler = function () {
      flag = false
      doc.removeClass(resizeClass)
    }
    var resizeHandler = function () {
      if (!flag) {
        flag = true
        doc.addClass(resizeClass)
      }
      clearTimeout(timer)
      timer = setTimeout(removeClassHandler, 500)
    }
    win.on("resize orientationchange", resizeHandler)
  }

  $.fn.mobileNav = function (opt) {
    var args = Array.prototype.slice.call(arguments)
    var method = args[0]

    return this.each(function () {
      var $container = jQuery(this)
      var instance = $container.data("MobileNav")

      if (typeof opt === "object" || typeof opt === "undefined") {
        $container.data(
          "MobileNav",
          new MobileNav(
            $.extend(
              {
                container: this,
              },
              opt
            )
          )
        )
      } else if (typeof method === "string" && instance) {
        if (typeof instance[method] === "function") {
          args.shift()
          instance[method].apply(instance, args)
        }
      }
    })
  }
})(jQuery)

initInViewport()
initMobileNav()

var stickyOffset = $(".header").offset()
var $contentDivs = $(".section")

function updateNavbarBg() {
  $contentDivs.each(function (k) {
    var _thisOffset = $(this).offset()
    var _actPosition = _thisOffset.top - $(window).scrollTop()
    if (
      _actPosition < stickyOffset.top + $(".header").height() &&
      _actPosition + $(this).height() - $(".header").height() > 0
    ) {
      var _myclass = $(this)
        .attr("class")
        .split(" ")
        .filter(str => str.startsWith("s_"))[0]
      $(".header")
        .removeClass(
          "s_white s_yellow s_black s_purple s_grey s_red s_blue s_beige s_orange s_green navbar-inverted"
        )
        .addClass(_myclass)
      if (["s_purple", "s_black"].includes(_myclass))
        $(".header").addClass("navbar-inverted")
      return false
    }
  })
}

$(document).scroll(updateNavbarBg)

// updateNavbarBg()
