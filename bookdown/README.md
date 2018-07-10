### How to build bookdown tutorial and copy to "../docs/tutorial" 

You need to set the "bookdown" directory as working directory in order to 
build the book. Then run the following code

```
fs::dir_delete("../docs/tutorial")
rmarkdown::render_site(encoding = 'UTF-8')
fs::dir_copy(path = "tutorial",new_path = "../docs/tutorial")
fs::dir_delete(path = "tutorial")

```

Subsequently reset the working directory to the root of this repo and 
run: 

```
pkgdown::build_site()

```
