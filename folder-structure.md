# Folder structure

# pages

All the pages goes in the main `src/pages` folder.
By Gatsby convention, every file in this folder is turned into a route.
Except the following naming conventions
.**_
\__**

Each page has some sections which targets to show the user some info/content.
If the page you creating goes 200+ lines, then it's recommended to do the following.
Add a \_components directory to the main root foler of the page.
Then, keep all the components related to that page in that folder
with following this name convention:

1. Start each section file name with "\_" -- exampe \_mainHero.js
2. Start each component file name with "." -- example .CustomBgImgWithStrip.js
3. Export all the main sections from the \_index file in the \_components folder.
   Note: There should be no page file in the \_components folder.
   Note: If you ever need to use the \_components route then consider changing the folder strucutre
   there as you best fit. Overall, follow the above convention.
