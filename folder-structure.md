# Folder structure

# pages

All pages go in the `src/views` folder.
We only use the `src/pages` folder for Routing purpose:
Gatsby creates a page for each file present in
the `src/pages` folder with the folder structure hierarchy as routes.
Example:
If we have a file in `src/pages/services/peoples` then,
it will create a page at `tweag.io/services/peoples`

To do this, we will first import the file from the `src/views`
that is being used for rendering of this page, and then
export it from the `src/pages` folder.
