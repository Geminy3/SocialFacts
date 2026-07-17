# get_cramer

Get Cramer or YuleQ's tab

## Usage

``` r
cramer_tab(var = NULL, metric = "cramer", data = NULL, weight = NULL)
```

## Arguments

- var:

  A list of variable name you want to compute "metric" on

- metric:

  Choose between 'Cramer' or 'YuleQ' parameter to select one of this
  metrics

- data:

  The data table you want to compute 'metric' parameter on

- weight:

  The weight vector if there is one in the dataframe

## Value

A list of gt information

## Examples

``` r
data <- graphPAF::Hordaland_data
cramer_tab(c("y", "urban.rural"), metric = 'cramer', data = data)
#> $chi2
#> <div id="mwlefoodfi" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
#>   <style>#mwlefoodfi table {
#>   font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
#>   -webkit-font-smoothing: antialiased;
#>   -moz-osx-font-smoothing: grayscale;
#> }
#> 
#> #mwlefoodfi thead, #mwlefoodfi tbody, #mwlefoodfi tfoot, #mwlefoodfi tr, #mwlefoodfi td, #mwlefoodfi th {
#>   border-style: none;
#> }
#> 
#> #mwlefoodfi p {
#>   margin: 0;
#>   padding: 0;
#> }
#> 
#> #mwlefoodfi .gt_table {
#>   display: table;
#>   border-collapse: collapse;
#>   line-height: normal;
#>   margin-left: auto;
#>   margin-right: auto;
#>   color: #333333;
#>   font-size: 16px;
#>   font-weight: normal;
#>   font-style: normal;
#>   background-color: #FFFFFF;
#>   width: auto;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #A8A8A8;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #A8A8A8;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#> }
#> 
#> #mwlefoodfi .gt_caption {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#> }
#> 
#> #mwlefoodfi .gt_title {
#>   color: #333333;
#>   font-size: 125%;
#>   font-weight: initial;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-color: #FFFFFF;
#>   border-bottom-width: 0;
#> }
#> 
#> #mwlefoodfi .gt_subtitle {
#>   color: #333333;
#>   font-size: 85%;
#>   font-weight: initial;
#>   padding-top: 3px;
#>   padding-bottom: 5px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-color: #FFFFFF;
#>   border-top-width: 0;
#> }
#> 
#> #mwlefoodfi .gt_heading {
#>   background-color: #FFFFFF;
#>   text-align: center;
#>   border-bottom-color: #FFFFFF;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #mwlefoodfi .gt_bottom_border {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #mwlefoodfi .gt_col_headings {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #mwlefoodfi .gt_col_heading {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: normal;
#>   text-transform: inherit;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: bottom;
#>   padding-top: 5px;
#>   padding-bottom: 6px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   overflow-x: hidden;
#> }
#> 
#> #mwlefoodfi .gt_column_spanner_outer {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: normal;
#>   text-transform: inherit;
#>   padding-top: 0;
#>   padding-bottom: 0;
#>   padding-left: 4px;
#>   padding-right: 4px;
#> }
#> 
#> #mwlefoodfi .gt_column_spanner_outer:first-child {
#>   padding-left: 0;
#> }
#> 
#> #mwlefoodfi .gt_column_spanner_outer:last-child {
#>   padding-right: 0;
#> }
#> 
#> #mwlefoodfi .gt_column_spanner {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   vertical-align: bottom;
#>   padding-top: 5px;
#>   padding-bottom: 5px;
#>   overflow-x: hidden;
#>   display: inline-block;
#>   width: 100%;
#> }
#> 
#> #mwlefoodfi .gt_spanner_row {
#>   border-bottom-style: hidden;
#> }
#> 
#> #mwlefoodfi .gt_group_heading {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: middle;
#>   text-align: left;
#> }
#> 
#> #mwlefoodfi .gt_empty_group_heading {
#>   padding: 0.5px;
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   vertical-align: middle;
#> }
#> 
#> #mwlefoodfi .gt_from_md > :first-child {
#>   margin-top: 0;
#> }
#> 
#> #mwlefoodfi .gt_from_md > :last-child {
#>   margin-bottom: 0;
#> }
#> 
#> #mwlefoodfi .gt_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   margin: 10px;
#>   border-top-style: solid;
#>   border-top-width: 1px;
#>   border-top-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: middle;
#>   overflow-x: hidden;
#> }
#> 
#> #mwlefoodfi .gt_stub {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-right-style: solid;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #mwlefoodfi .gt_stub_row_group {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-right-style: solid;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   vertical-align: top;
#> }
#> 
#> #mwlefoodfi .gt_row_group_first td {
#>   border-top-width: 2px;
#> }
#> 
#> #mwlefoodfi .gt_row_group_first th {
#>   border-top-width: 2px;
#> }
#> 
#> #mwlefoodfi .gt_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #mwlefoodfi .gt_first_summary_row {
#>   border-top-style: solid;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #mwlefoodfi .gt_first_summary_row.thick {
#>   border-top-width: 2px;
#> }
#> 
#> #mwlefoodfi .gt_last_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #mwlefoodfi .gt_grand_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #mwlefoodfi .gt_first_grand_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-style: double;
#>   border-top-width: 6px;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #mwlefoodfi .gt_last_grand_summary_row_top {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: double;
#>   border-bottom-width: 6px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #mwlefoodfi .gt_striped {
#>   background-color: rgba(128, 128, 128, 0.05);
#> }
#> 
#> #mwlefoodfi .gt_table_body {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #mwlefoodfi .gt_footnotes {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   border-bottom-style: none;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #mwlefoodfi .gt_footnote {
#>   margin: 0px;
#>   font-size: 90%;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #mwlefoodfi .gt_sourcenotes {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   border-bottom-style: none;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #mwlefoodfi .gt_sourcenote {
#>   font-size: 90%;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #mwlefoodfi .gt_left {
#>   text-align: left;
#> }
#> 
#> #mwlefoodfi .gt_center {
#>   text-align: center;
#> }
#> 
#> #mwlefoodfi .gt_right {
#>   text-align: right;
#>   font-variant-numeric: tabular-nums;
#> }
#> 
#> #mwlefoodfi .gt_font_normal {
#>   font-weight: normal;
#> }
#> 
#> #mwlefoodfi .gt_font_bold {
#>   font-weight: bold;
#> }
#> 
#> #mwlefoodfi .gt_font_italic {
#>   font-style: italic;
#> }
#> 
#> #mwlefoodfi .gt_super {
#>   font-size: 65%;
#> }
#> 
#> #mwlefoodfi .gt_footnote_marks {
#>   font-size: 75%;
#>   vertical-align: 0.4em;
#>   position: initial;
#> }
#> 
#> #mwlefoodfi .gt_asterisk {
#>   font-size: 100%;
#>   vertical-align: 0;
#> }
#> 
#> #mwlefoodfi .gt_indent_1 {
#>   text-indent: 5px;
#> }
#> 
#> #mwlefoodfi .gt_indent_2 {
#>   text-indent: 10px;
#> }
#> 
#> #mwlefoodfi .gt_indent_3 {
#>   text-indent: 15px;
#> }
#> 
#> #mwlefoodfi .gt_indent_4 {
#>   text-indent: 20px;
#> }
#> 
#> #mwlefoodfi .gt_indent_5 {
#>   text-indent: 25px;
#> }
#> 
#> #mwlefoodfi .katex-display {
#>   display: inline-flex !important;
#>   margin-bottom: 0.75em !important;
#> }
#> 
#> #mwlefoodfi div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
#>   height: 0px !important;
#> }
#> </style>
#>   <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
#>   <thead>
#>     <tr class="gt_heading">
#>       <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Chi2 des variables</td>
#>     </tr>
#>     <tr class="gt_heading">
#>       <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style><span class='gt_from_md'>arrondi - <link rel="stylesheet" type="text/css" href="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css" data-external="1">
#> <span class="katex"><span class="katex-mathml"><math xmlns="http://www.w3.org/1998/Math/MathML"><semantics><mrow><mn>1</mn><msup><mn>0</mn><mrow><mo>−</mo><mn>5</mn></mrow></msup></mrow><annotation encoding="application/x-tex">10^{-5}</annotation></semantics></math></span><span class="katex-html" aria-hidden="true"><span class="base"><span class="strut" style="height:0.8141em;"></span><span class="mord">1</span><span class="mord"><span class="mord">0</span><span class="msupsub"><span class="vlist-t"><span class="vlist-r"><span class="vlist" style="height:0.8141em;"><span style="top:-3.063em;margin-right:0.05em;"><span class="pstrut" style="height:2.7em;"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight"><span class="mord mtight">−</span><span class="mord mtight">5</span></span></span></span></span></span></span></span></span></span></span></span></span></td>
#>     </tr>
#>     <tr class="gt_col_headings">
#>       <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a::stub"></th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="y">y</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="urban.rural">urban.rural</th>
#>     </tr>
#>   </thead>
#>   <tbody class="gt_table_body">
#>     <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">y</th>
#> <td headers="stub_1_1 y" class="gt_row gt_right" style="background-color: #A020F0; color: #FFFFFF;">0</td>
#> <td headers="stub_1_1 urban.rural" class="gt_row gt_right" style="background-color: #A020F0; color: #FFFFFF;">0</td></tr>
#>     <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">urban.rural</th>
#> <td headers="stub_1_2 y" class="gt_row gt_right" style="background-color: #A020F0; color: #FFFFFF;">0</td>
#> <td headers="stub_1_2 urban.rural" class="gt_row gt_right" style="background-color: #A020F0; color: #FFFFFF;">0</td></tr>
#>   </tbody>
#>   
#> </table>
#> </div>
#> 
#> $metric
#> <div id="wkkeulbfdp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
#>   <style>#wkkeulbfdp table {
#>   font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
#>   -webkit-font-smoothing: antialiased;
#>   -moz-osx-font-smoothing: grayscale;
#> }
#> 
#> #wkkeulbfdp thead, #wkkeulbfdp tbody, #wkkeulbfdp tfoot, #wkkeulbfdp tr, #wkkeulbfdp td, #wkkeulbfdp th {
#>   border-style: none;
#> }
#> 
#> #wkkeulbfdp p {
#>   margin: 0;
#>   padding: 0;
#> }
#> 
#> #wkkeulbfdp .gt_table {
#>   display: table;
#>   border-collapse: collapse;
#>   line-height: normal;
#>   margin-left: auto;
#>   margin-right: auto;
#>   color: #333333;
#>   font-size: 16px;
#>   font-weight: normal;
#>   font-style: normal;
#>   background-color: #FFFFFF;
#>   width: auto;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #A8A8A8;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #A8A8A8;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_caption {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#> }
#> 
#> #wkkeulbfdp .gt_title {
#>   color: #333333;
#>   font-size: 125%;
#>   font-weight: initial;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-color: #FFFFFF;
#>   border-bottom-width: 0;
#> }
#> 
#> #wkkeulbfdp .gt_subtitle {
#>   color: #333333;
#>   font-size: 85%;
#>   font-weight: initial;
#>   padding-top: 3px;
#>   padding-bottom: 5px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-color: #FFFFFF;
#>   border-top-width: 0;
#> }
#> 
#> #wkkeulbfdp .gt_heading {
#>   background-color: #FFFFFF;
#>   text-align: center;
#>   border-bottom-color: #FFFFFF;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_bottom_border {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_col_headings {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_col_heading {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: normal;
#>   text-transform: inherit;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: bottom;
#>   padding-top: 5px;
#>   padding-bottom: 6px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   overflow-x: hidden;
#> }
#> 
#> #wkkeulbfdp .gt_column_spanner_outer {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: normal;
#>   text-transform: inherit;
#>   padding-top: 0;
#>   padding-bottom: 0;
#>   padding-left: 4px;
#>   padding-right: 4px;
#> }
#> 
#> #wkkeulbfdp .gt_column_spanner_outer:first-child {
#>   padding-left: 0;
#> }
#> 
#> #wkkeulbfdp .gt_column_spanner_outer:last-child {
#>   padding-right: 0;
#> }
#> 
#> #wkkeulbfdp .gt_column_spanner {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   vertical-align: bottom;
#>   padding-top: 5px;
#>   padding-bottom: 5px;
#>   overflow-x: hidden;
#>   display: inline-block;
#>   width: 100%;
#> }
#> 
#> #wkkeulbfdp .gt_spanner_row {
#>   border-bottom-style: hidden;
#> }
#> 
#> #wkkeulbfdp .gt_group_heading {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: middle;
#>   text-align: left;
#> }
#> 
#> #wkkeulbfdp .gt_empty_group_heading {
#>   padding: 0.5px;
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   vertical-align: middle;
#> }
#> 
#> #wkkeulbfdp .gt_from_md > :first-child {
#>   margin-top: 0;
#> }
#> 
#> #wkkeulbfdp .gt_from_md > :last-child {
#>   margin-bottom: 0;
#> }
#> 
#> #wkkeulbfdp .gt_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   margin: 10px;
#>   border-top-style: solid;
#>   border-top-width: 1px;
#>   border-top-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: middle;
#>   overflow-x: hidden;
#> }
#> 
#> #wkkeulbfdp .gt_stub {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-right-style: solid;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #wkkeulbfdp .gt_stub_row_group {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-right-style: solid;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   vertical-align: top;
#> }
#> 
#> #wkkeulbfdp .gt_row_group_first td {
#>   border-top-width: 2px;
#> }
#> 
#> #wkkeulbfdp .gt_row_group_first th {
#>   border-top-width: 2px;
#> }
#> 
#> #wkkeulbfdp .gt_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #wkkeulbfdp .gt_first_summary_row {
#>   border-top-style: solid;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_first_summary_row.thick {
#>   border-top-width: 2px;
#> }
#> 
#> #wkkeulbfdp .gt_last_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_grand_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #wkkeulbfdp .gt_first_grand_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-style: double;
#>   border-top-width: 6px;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_last_grand_summary_row_top {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: double;
#>   border-bottom-width: 6px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_striped {
#>   background-color: rgba(128, 128, 128, 0.05);
#> }
#> 
#> #wkkeulbfdp .gt_table_body {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_footnotes {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   border-bottom-style: none;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_footnote {
#>   margin: 0px;
#>   font-size: 90%;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #wkkeulbfdp .gt_sourcenotes {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   border-bottom-style: none;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_sourcenote {
#>   font-size: 90%;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #wkkeulbfdp .gt_left {
#>   text-align: left;
#> }
#> 
#> #wkkeulbfdp .gt_center {
#>   text-align: center;
#> }
#> 
#> #wkkeulbfdp .gt_right {
#>   text-align: right;
#>   font-variant-numeric: tabular-nums;
#> }
#> 
#> #wkkeulbfdp .gt_font_normal {
#>   font-weight: normal;
#> }
#> 
#> #wkkeulbfdp .gt_font_bold {
#>   font-weight: bold;
#> }
#> 
#> #wkkeulbfdp .gt_font_italic {
#>   font-style: italic;
#> }
#> 
#> #wkkeulbfdp .gt_super {
#>   font-size: 65%;
#> }
#> 
#> #wkkeulbfdp .gt_footnote_marks {
#>   font-size: 75%;
#>   vertical-align: 0.4em;
#>   position: initial;
#> }
#> 
#> #wkkeulbfdp .gt_asterisk {
#>   font-size: 100%;
#>   vertical-align: 0;
#> }
#> 
#> #wkkeulbfdp .gt_indent_1 {
#>   text-indent: 5px;
#> }
#> 
#> #wkkeulbfdp .gt_indent_2 {
#>   text-indent: 10px;
#> }
#> 
#> #wkkeulbfdp .gt_indent_3 {
#>   text-indent: 15px;
#> }
#> 
#> #wkkeulbfdp .gt_indent_4 {
#>   text-indent: 20px;
#> }
#> 
#> #wkkeulbfdp .gt_indent_5 {
#>   text-indent: 25px;
#> }
#> 
#> #wkkeulbfdp .katex-display {
#>   display: inline-flex !important;
#>   margin-bottom: 0.75em !important;
#> }
#> 
#> #wkkeulbfdp div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
#>   height: 0px !important;
#> }
#> </style>
#>   <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
#>   <thead>
#>     <tr class="gt_heading">
#>       <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Correlation des variables</td>
#>     </tr>
#>     <tr class="gt_heading">
#>       <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>cramer</td>
#>     </tr>
#>     <tr class="gt_col_headings">
#>       <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a::stub"></th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="y">y</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="urban.rural">urban.rural</th>
#>     </tr>
#>   </thead>
#>   <tbody class="gt_table_body">
#>     <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">y</th>
#> <td headers="stub_1_1 y" class="gt_row gt_right" style="background-color: #808080; color: #FFFFFF;">NA</td>
#> <td headers="stub_1_1 urban.rural" class="gt_row gt_right" style="background-color: #B740DB; color: #FFFFFF;">0.12231</td></tr>
#>     <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">urban.rural</th>
#> <td headers="stub_1_2 y" class="gt_row gt_right" style="background-color: #B740DB; color: #FFFFFF;">0.12231</td>
#> <td headers="stub_1_2 urban.rural" class="gt_row gt_right" style="background-color: #808080; color: #FFFFFF;">NA</td></tr>
#>   </tbody>
#>   
#> </table>
#> </div>
#> 
```
