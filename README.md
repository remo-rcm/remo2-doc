# remo documentation

[![Documentation Status](https://readthedocs.org/projects/remo2-doc/badge/?version=latest)](https://remo2-doc.readthedocs.io/en/latest/?badge=latest)

The documentation for remo2 is using [sphinx](https://docs.readthedocs.io/en/stable/intro/getting-started-with-sphinx.html).
If you want to build the documentation, you can choose between `html` or `pdf`, using:

```
make html
make latexpdf
```

## Contributions

You are welcome to contribute documentation by simply adding a file it in the `rst` format.
The source files are located in the `source` directory. The start page is defined
in the `index.rst` file where you can include new chapters. If you are adding
an external contribution, you can add it to the `externals.rst` index page and
simply write your documentation into a plain `rst` file. Formatting it nicely
is not important and can be done afterwards. If you are interested in formatting
your text, have a look at the `syntax-examples.rst` file. Remember that you can also edit the rst files in the gitlab.
