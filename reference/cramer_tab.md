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
#> <div id="lefoodfiee" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
#>   <style>#lefoodfiee table {
#>   font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
#>   -webkit-font-smoothing: antialiased;
#>   -moz-osx-font-smoothing: grayscale;
#> }
#> 
#> #lefoodfiee thead, #lefoodfiee tbody, #lefoodfiee tfoot, #lefoodfiee tr, #lefoodfiee td, #lefoodfiee th {
#>   border-style: none;
#> }
#> 
#> #lefoodfiee p {
#>   margin: 0;
#>   padding: 0;
#> }
#> 
#> #lefoodfiee .gt_table {
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
#> #lefoodfiee .gt_caption {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#> }
#> 
#> #lefoodfiee .gt_title {
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
#> #lefoodfiee .gt_subtitle {
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
#> #lefoodfiee .gt_heading {
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
#> #lefoodfiee .gt_bottom_border {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #lefoodfiee .gt_col_headings {
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
#> #lefoodfiee .gt_col_heading {
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
#> #lefoodfiee .gt_column_spanner_outer {
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
#> #lefoodfiee .gt_column_spanner_outer:first-child {
#>   padding-left: 0;
#> }
#> 
#> #lefoodfiee .gt_column_spanner_outer:last-child {
#>   padding-right: 0;
#> }
#> 
#> #lefoodfiee .gt_column_spanner {
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
#> #lefoodfiee .gt_spanner_row {
#>   border-bottom-style: hidden;
#> }
#> 
#> #lefoodfiee .gt_group_heading {
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
#> #lefoodfiee .gt_empty_group_heading {
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
#> #lefoodfiee .gt_from_md > :first-child {
#>   margin-top: 0;
#> }
#> 
#> #lefoodfiee .gt_from_md > :last-child {
#>   margin-bottom: 0;
#> }
#> 
#> #lefoodfiee .gt_row {
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
#> #lefoodfiee .gt_stub {
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
#> #lefoodfiee .gt_stub_row_group {
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
#> #lefoodfiee .gt_row_group_first td {
#>   border-top-width: 2px;
#> }
#> 
#> #lefoodfiee .gt_row_group_first th {
#>   border-top-width: 2px;
#> }
#> 
#> #lefoodfiee .gt_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #lefoodfiee .gt_first_summary_row {
#>   border-top-style: solid;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #lefoodfiee .gt_first_summary_row.thick {
#>   border-top-width: 2px;
#> }
#> 
#> #lefoodfiee .gt_last_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #lefoodfiee .gt_grand_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #lefoodfiee .gt_first_grand_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-style: double;
#>   border-top-width: 6px;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #lefoodfiee .gt_last_grand_summary_row_top {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: double;
#>   border-bottom-width: 6px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #lefoodfiee .gt_striped {
#>   background-color: rgba(128, 128, 128, 0.05);
#> }
#> 
#> #lefoodfiee .gt_table_body {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #lefoodfiee .gt_footnotes {
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
#> #lefoodfiee .gt_footnote {
#>   margin: 0px;
#>   font-size: 90%;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #lefoodfiee .gt_sourcenotes {
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
#> #lefoodfiee .gt_sourcenote {
#>   font-size: 90%;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #lefoodfiee .gt_left {
#>   text-align: left;
#> }
#> 
#> #lefoodfiee .gt_center {
#>   text-align: center;
#> }
#> 
#> #lefoodfiee .gt_right {
#>   text-align: right;
#>   font-variant-numeric: tabular-nums;
#> }
#> 
#> #lefoodfiee .gt_font_normal {
#>   font-weight: normal;
#> }
#> 
#> #lefoodfiee .gt_font_bold {
#>   font-weight: bold;
#> }
#> 
#> #lefoodfiee .gt_font_italic {
#>   font-style: italic;
#> }
#> 
#> #lefoodfiee .gt_super {
#>   font-size: 65%;
#> }
#> 
#> #lefoodfiee .gt_footnote_marks {
#>   font-size: 75%;
#>   vertical-align: 0.4em;
#>   position: initial;
#> }
#> 
#> #lefoodfiee .gt_asterisk {
#>   font-size: 100%;
#>   vertical-align: 0;
#> }
#> 
#> #lefoodfiee .gt_indent_1 {
#>   text-indent: 5px;
#> }
#> 
#> #lefoodfiee .gt_indent_2 {
#>   text-indent: 10px;
#> }
#> 
#> #lefoodfiee .gt_indent_3 {
#>   text-indent: 15px;
#> }
#> 
#> #lefoodfiee .gt_indent_4 {
#>   text-indent: 20px;
#> }
#> 
#> #lefoodfiee .gt_indent_5 {
#>   text-indent: 25px;
#> }
#> 
#> #lefoodfiee .katex-display {
#>   display: inline-flex !important;
#>   margin-bottom: 0.75em !important;
#> }
#> 
#> #lefoodfiee div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
#> <div id="keulbfdpbc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
#>   <style>#keulbfdpbc table {
#>   font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
#>   -webkit-font-smoothing: antialiased;
#>   -moz-osx-font-smoothing: grayscale;
#> }
#> 
#> #keulbfdpbc thead, #keulbfdpbc tbody, #keulbfdpbc tfoot, #keulbfdpbc tr, #keulbfdpbc td, #keulbfdpbc th {
#>   border-style: none;
#> }
#> 
#> #keulbfdpbc p {
#>   margin: 0;
#>   padding: 0;
#> }
#> 
#> #keulbfdpbc .gt_table {
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
#> #keulbfdpbc .gt_caption {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#> }
#> 
#> #keulbfdpbc .gt_title {
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
#> #keulbfdpbc .gt_subtitle {
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
#> #keulbfdpbc .gt_heading {
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
#> #keulbfdpbc .gt_bottom_border {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #keulbfdpbc .gt_col_headings {
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
#> #keulbfdpbc .gt_col_heading {
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
#> #keulbfdpbc .gt_column_spanner_outer {
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
#> #keulbfdpbc .gt_column_spanner_outer:first-child {
#>   padding-left: 0;
#> }
#> 
#> #keulbfdpbc .gt_column_spanner_outer:last-child {
#>   padding-right: 0;
#> }
#> 
#> #keulbfdpbc .gt_column_spanner {
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
#> #keulbfdpbc .gt_spanner_row {
#>   border-bottom-style: hidden;
#> }
#> 
#> #keulbfdpbc .gt_group_heading {
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
#> #keulbfdpbc .gt_empty_group_heading {
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
#> #keulbfdpbc .gt_from_md > :first-child {
#>   margin-top: 0;
#> }
#> 
#> #keulbfdpbc .gt_from_md > :last-child {
#>   margin-bottom: 0;
#> }
#> 
#> #keulbfdpbc .gt_row {
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
#> #keulbfdpbc .gt_stub {
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
#> #keulbfdpbc .gt_stub_row_group {
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
#> #keulbfdpbc .gt_row_group_first td {
#>   border-top-width: 2px;
#> }
#> 
#> #keulbfdpbc .gt_row_group_first th {
#>   border-top-width: 2px;
#> }
#> 
#> #keulbfdpbc .gt_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #keulbfdpbc .gt_first_summary_row {
#>   border-top-style: solid;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #keulbfdpbc .gt_first_summary_row.thick {
#>   border-top-width: 2px;
#> }
#> 
#> #keulbfdpbc .gt_last_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #keulbfdpbc .gt_grand_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #keulbfdpbc .gt_first_grand_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-style: double;
#>   border-top-width: 6px;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #keulbfdpbc .gt_last_grand_summary_row_top {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: double;
#>   border-bottom-width: 6px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #keulbfdpbc .gt_striped {
#>   background-color: rgba(128, 128, 128, 0.05);
#> }
#> 
#> #keulbfdpbc .gt_table_body {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #keulbfdpbc .gt_footnotes {
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
#> #keulbfdpbc .gt_footnote {
#>   margin: 0px;
#>   font-size: 90%;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #keulbfdpbc .gt_sourcenotes {
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
#> #keulbfdpbc .gt_sourcenote {
#>   font-size: 90%;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #keulbfdpbc .gt_left {
#>   text-align: left;
#> }
#> 
#> #keulbfdpbc .gt_center {
#>   text-align: center;
#> }
#> 
#> #keulbfdpbc .gt_right {
#>   text-align: right;
#>   font-variant-numeric: tabular-nums;
#> }
#> 
#> #keulbfdpbc .gt_font_normal {
#>   font-weight: normal;
#> }
#> 
#> #keulbfdpbc .gt_font_bold {
#>   font-weight: bold;
#> }
#> 
#> #keulbfdpbc .gt_font_italic {
#>   font-style: italic;
#> }
#> 
#> #keulbfdpbc .gt_super {
#>   font-size: 65%;
#> }
#> 
#> #keulbfdpbc .gt_footnote_marks {
#>   font-size: 75%;
#>   vertical-align: 0.4em;
#>   position: initial;
#> }
#> 
#> #keulbfdpbc .gt_asterisk {
#>   font-size: 100%;
#>   vertical-align: 0;
#> }
#> 
#> #keulbfdpbc .gt_indent_1 {
#>   text-indent: 5px;
#> }
#> 
#> #keulbfdpbc .gt_indent_2 {
#>   text-indent: 10px;
#> }
#> 
#> #keulbfdpbc .gt_indent_3 {
#>   text-indent: 15px;
#> }
#> 
#> #keulbfdpbc .gt_indent_4 {
#>   text-indent: 20px;
#> }
#> 
#> #keulbfdpbc .gt_indent_5 {
#>   text-indent: 25px;
#> }
#> 
#> #keulbfdpbc .katex-display {
#>   display: inline-flex !important;
#>   margin-bottom: 0.75em !important;
#> }
#> 
#> #keulbfdpbc div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
