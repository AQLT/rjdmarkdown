# Print the diagnostics

Function to print diagnostics tests on the quality of the pre-processing
and the decomposition

## Usage

``` r
print_diagnostics(
  x,
  format = knitr::opts_knit$get("rmarkdown.pandoc.to"),
  signif.stars = TRUE,
  tests = c("mean", "skewness", "kurtosis", "ljung box",
    "ljung box (residuals at seasonal lags)", "ljung box (squared residuals)",
    "qs test on sa", "qs test on i", "f-test on sa (seasonal dummies)",
    "f-test on i (seasonal dummies)", "Residual seasonality (entire series)",
    "Residual seasonality (last 3 years)", "f-test on sa (td)", "f-test on i (td)"),
  digits = 3,
  decimal.mark = getOption("OutDec"),
  booktabs = TRUE,
  ...
)
```

## Arguments

- x:

  the object to print.

- format:

  output format: `"latex"` or `"html"`.

- signif.stars:

  logical; if `TRUE`, p-values are additionally encoded visually as
  ‘significance stars’ in order to help scanning of long coefficient
  tables

- tests:

  characters containing the names of the tests to print.

- digits:

  number of digits after the decimal point.

- decimal.mark:

  the character to be used to indicate the numeric decimal point.

- booktabs:

  boolean indicating whether to use or not the booktabs package (when
  `format = "latex"`).

- ...:

  unused arguments.

## Examples

``` r
ipi <- RJDemetra::ipi_c_eu[, "FR"]

jsa_x13 <- RJDemetra::jx13(ipi)
print_diagnostics(jsa_x13, format = "latex")
#> \begin{table}[H]
#> \centering
#> \caption{Diagnostics tests}
#> \centering
#> \begin{tabular}[t]{l|c|c}
#> \hline
#>   & $\mathbb P (> \lvert t \rvert)$ & \\
#> \hline
#> mean & 0.899 & \\
#> \hline
#> skewness & 0.880 & \\
#> \hline
#> kurtosis & 0.034 & *\\
#> \hline
#> ljung box & 0.000 & ***\\
#> \hline
#> ljung box (residuals at seasonal lags) & 0.212 & \\
#> \hline
#> ljung box (squared residuals) & 0.024 & *\\
#> \hline
#> qs test on sa & 0.985 & \\
#> \hline
#> qs test on i & 0.865 & \\
#> \hline
#> f-test on sa (seasonal dummies) & 0.958 & \\
#> \hline
#> f-test on i (seasonal dummies) & 0.893 & \\
#> \hline
#> Residual seasonality (entire series) & 0.876 & \\
#> \hline
#> Residual seasonality (last 3 years) & 0.906 & \\
#> \hline
#> f-test on sa (td) & 0.987 & \\
#> \hline
#> f-test on i (td) & 0.993 & \\
#> \hline
#> \multicolumn{3}{l}{\rule{0pt}{1em}\textbf{Signif. codes: }0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1}\\
#> \end{tabular}
#> \end{table}

# \donttest{
sa_ts <- RJDemetra::tramoseats(ipi)
print_diagnostics(sa_ts, format = "html")
#> <table class="table" style="margin-left: auto; margin-right: auto;border-bottom: 0;">
#> <caption>Diagnostics tests</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;">   </th>
#>    <th style="text-align:center;"> P (&gt; | t|) </th>
#>    <th style="text-align:center;">  </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> mean </td>
#>    <td style="text-align:center;"> 0.988 </td>
#>    <td style="text-align:center;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> skewness </td>
#>    <td style="text-align:center;"> 0.413 </td>
#>    <td style="text-align:center;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> kurtosis </td>
#>    <td style="text-align:center;"> 0.095 </td>
#>    <td style="text-align:center;"> . </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> ljung box </td>
#>    <td style="text-align:center;"> 0.010 </td>
#>    <td style="text-align:center;"> ** </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> ljung box (residuals at seasonal lags) </td>
#>    <td style="text-align:center;"> 0.192 </td>
#>    <td style="text-align:center;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> ljung box (squared residuals) </td>
#>    <td style="text-align:center;"> 0.000 </td>
#>    <td style="text-align:center;"> *** </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> qs test on sa </td>
#>    <td style="text-align:center;"> 1.000 </td>
#>    <td style="text-align:center;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> qs test on i </td>
#>    <td style="text-align:center;"> 1.000 </td>
#>    <td style="text-align:center;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> f-test on sa (seasonal dummies) </td>
#>    <td style="text-align:center;"> 1.000 </td>
#>    <td style="text-align:center;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> f-test on i (seasonal dummies) </td>
#>    <td style="text-align:center;"> 1.000 </td>
#>    <td style="text-align:center;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Residual seasonality (entire series) </td>
#>    <td style="text-align:center;"> 1.000 </td>
#>    <td style="text-align:center;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Residual seasonality (last 3 years) </td>
#>    <td style="text-align:center;"> 0.974 </td>
#>    <td style="text-align:center;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> f-test on sa (td) </td>
#>    <td style="text-align:center;"> 0.152 </td>
#>    <td style="text-align:center;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> f-test on i (td) </td>
#>    <td style="text-align:center;"> 0.224 </td>
#>    <td style="text-align:center;">  </td>
#>   </tr>
#> </tbody>
#> <tfoot><tr><td style="padding: 0; " colspan="100%">
#> <sup></sup> <b>Signif. codes:</b> 0 '\*\*\*' 0.001 '\*\*' 0.01 '\*' 0.05 '.' 0.1 ' ' 1</td></tr></tfoot>
#> </table>
# }
```
