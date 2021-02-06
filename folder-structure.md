# Folder structure

# pages

All pages go in the main `src/pages` folder.
By Gatsby convention, every file in this folder is turned into a route.
Except for the following naming conventions

1. `/.*****`.
2. `/_*****`.

Each page has some sections to show the user some info/content.
If the page you are creating goes 200+ lines, then it's recommended to do the following:
Add a `/_components` directory to the main root folder of the page.
Then, keep all the components related to that page in that folder
with this name convention:

1. Start each section file name with `/_`. For instance, `/_mainHero.js`
2. Start each component file name with `.`. For instance, `/.CustomBgImgWithStrip.js`
3. Export all the main sections from the `/_index` file in the `/_components` folder.
   Note: There should be no page file in the `/_components` folder.
   Note: If you ever need to use the `/_components` route then consider changing the folder structure there as you best fit. Overall, follow the above convention.
