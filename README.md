# `remo2` Documenation

[![pipeline status](http://git.gerics.de/REMO/remo2-doc/badges/master/pipeline.svg)](http://git.gerics.de/REMO/remo2-doc/commits/master)

The documentation for remo2 comes is using [sphinx](https://docs.readthedocs.io/en/stable/intro/getting-started-with-sphinx.html).
If you want to build the documentation, you can choose between `html` or `pdf`, using:

```
make html
make pdf
```

## Contributions

You are welcome to contribute documentation by simply adding it in the `rst` format.
The source files are located in the `source` directory. The start page is defined
in the `index.rst` file where you can include new chapters. If you are adding
an external contribution, you can add it to the `externals.rst` index page and
simply write your documentation into a plain `rst` file. Formatting it nicely
is not important and can be done afterwards.
