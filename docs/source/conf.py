import os
import sys
sys.path.insert(0, os.path.abspath('.'))

import virdant_ext
import verilog_ext
import sphinx_rtd_theme
from sphinx.highlighting import lexers


# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'The Virdant Hardware Language'
copyright = '2026'


# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    "sphinx_rtd_theme",
    "virdant_ext",
    "sphinxext.opengraph",
]

templates_path = ['_templates']
exclude_patterns = []

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = "sphinx_rtd_theme"
html_theme_path = [sphinx_rtd_theme.get_html_theme_path()]

html_static_path = ['_static']


html_css_files = ['custom.css']

default_role = 'code'


# Add your custom lexer to the list of supported lexers
lexers['virdant'] = virdant_ext.VirdantLexer()
lexers['verilog'] = verilog_ext.VerilogLexer()
lexers['v'] = verilog_ext.VerilogLexer()

# Set the default lexer for your language
primary_domain = 'virdant'
highlight_language = 'virdant'
html_favicon = '_static/virdant-logo-white-transparent.svg'
html_logo = '_static/virdant-logo-white-transparent.svg'

# Open Graph configuration
ogp_site_url = "https://virdant.org"
ogp_image = "https://virdant.org/_static/virdant-logo-og.png"
ogp_site_name = "The Virdant Hardware Language"
