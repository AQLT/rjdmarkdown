# Print the pre-processing model

Function to print the pre-processing model

## Usage

``` r
print_preprocessing(
  x,
  format = knitr::opts_knit$get("rmarkdown.pandoc.to"),
  signif.stars = TRUE,
  digits = 3,
  decimal.mark = getOption("OutDec"),
  booktabs = TRUE,
  summary = TRUE,
  likelihood = TRUE,
  arima = TRUE,
  regression = TRUE,
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

- digits:

  number of digits after the decimal point.

- decimal.mark:

  the character to be used to indicate the numeric decimal point.

- booktabs:

  boolean indicating whether to use or not the booktabs package (when
  `format = "latex"`).

- summary:

  boolean indicating whether to use or not the summary section.

- likelihood:

  boolean indicating whether to use or not the likelihood section.

- arima:

  boolean indicating whether to use or not the arima section.

- regression:

  boolean indicating whether to use or not the regression section.

- ...:

  unused.

## Examples

``` r
ipi <- RJDemetra::ipi_c_eu[, "FR"]

sa_x13 <- RJDemetra::jx13(ipi)
print_preprocessing(sa_x13, format = "latex")
#> \underline{\textbf{Pre-processing (RegArima)}}
#> 
#> \underline{\textbf{Summary}}
#> 
#> 
#> 
#> 372 observations
#> 
#> Trading days effect (7 variables)
#> 
#> Easter [1] detected
#> 
#> 4 detected outliers
#> 
#> \underline{\textbf{Likelihood statistics}}
#> 
#> Number of effective observations = 359
#> 
#> Number of estimated parameters = 17
#> 
#> Loglikelihood = -799.084, AICc = 1633.964, BICc = 1.855
#> 
#> Standard error of the regression (ML estimate) = 2.218
#> 
#> \underline{\textbf{ARIMA model}}
#> 
#> \begin{table}[H]
#> \centering
#> \caption{ARIMA coefficients}
#> \centering
#> \begin{tabular}[t]{lccccc}
#> \toprule
#>   & Coefficients & Std. Error & T-stat & $\mathbb P (> \lvert t \rvert)$ & \\
#> \midrule
#> Phi(1) & 0.000 & 0.108 & 0.003 & 0.998 & \\
#> Phi(2) & 0.169 & 0.074 & 2.278 & 0.023 & *\\
#> Theta(1) & -0.549 & 0.102 & -5.396 & 0.000 & ***\\
#> BTheta(1) & -0.666 & 0.042 & -15.775 & 0.000 & ***\\
#> \bottomrule
#> \multicolumn{6}{l}{\rule{0pt}{1em}\textbf{Signif. codes: }0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1}\\
#> \multicolumn{6}{l}{\rule{0pt}{1em}ARIMA (2,1,1)(0,1,1)}\\
#> \end{tabular}
#> \end{table}
#> 
#> \underline{\textbf{Regression model}}
#> 
#> \begin{table}[H]
#> \centering
#> \caption{Regression coefficientss}
#> \centering
#> \begin{tabular}[t]{lccccc}
#> \toprule
#>   & Coefficients & Std. Error & T-stat & $\mathbb P (> \lvert t \rvert)$ & \\
#> \midrule
#> Monday & 0.559 & 0.228 & 2.453 & 0.015 & *\\
#> Tuesday & 0.882 & 0.228 & 3.864 & 0.000 & ***\\
#> Wednesday & 1.040 & 0.229 & 4.535 & 0.000 & ***\\
#> Thursday & 0.049 & 0.229 & 0.215 & 0.830 & \\
#> Friday & 0.911 & 0.230 & 3.964 & 0.000 & ***\\
#> \addlinespace
#> Saturday & -1.578 & 0.228 & -6.927 & 0.000 & ***\\
#> Leap year & 2.154 & 0.705 & 3.054 & 0.002 & **\\
#> Easter [1] & -2.380 & 0.454 & -5.242 & 0.000 & ***\\
#> TC (4-2020) & -35.592 & 2.173 & -16.377 & 0.000 & ***\\
#> AO (3-2020) & -20.890 & 2.180 & -9.582 & 0.000 & ***\\
#> \addlinespace
#> AO (5-2011) & 13.499 & 1.857 & 7.269 & 0.000 & ***\\
#> LS (11-2008) & -12.549 & 1.636 & -7.673 & 0.000 & ***\\
#> \bottomrule
#> \multicolumn{6}{l}{\rule{0pt}{1em}\textbf{Signif. codes: }0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1}\\
#> \end{tabular}
#> \end{table}
#> 

# \donttest{
sa_ts <- RJDemetra::tramoseats(ipi)
print_preprocessing(sa_ts, format = "html")
#> <u><b>Pre-processing (Tramo)</b> </u>
#> 
#> <u><b>Summary</b></u>
#> 
#> 
#> 
#> 372 observations
#> 
#> Trading days effect (2 variables)
#> 
#> Easter [6] detected
#> 
#> 4 detected outliers
#> 
#> <u><b>Likelihood statistics</b></u>
#> 
#> Number of effective observations = 359
#> 
#> Number of estimated parameters = 11
#> 
#> Loglikelihood = -816.075, AICc = 1654.912, BICc = 1.852
#> 
#> Standard error of the regression (ML estimate) = 2.326
#> 
#> <u><b>ARIMA model</b></u>
#> 
#> <table class="table" style="margin-left: auto; margin-right: auto;border-bottom: 0;">
#> <caption>ARIMA coefficients</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;">   </th>
#>    <th style="text-align:center;"> Coefficients </th>
#>    <th style="text-align:center;"> Std. Error </th>
#>    <th style="text-align:center;"> T-stat </th>
#>    <th style="text-align:center;"> P (&gt; | t|) </th>
#>    <th style="text-align:center;">  </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> Phi(1) </td>
#>    <td style="text-align:center;"> 0.403 </td>
#>    <td style="text-align:center;"> 0.051 </td>
#>    <td style="text-align:center;"> 7.845 </td>
#>    <td style="text-align:center;"> 0.000 </td>
#>    <td style="text-align:center;"> *** </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Phi(2) </td>
#>    <td style="text-align:center;"> 0.288 </td>
#>    <td style="text-align:center;"> 0.051 </td>
#>    <td style="text-align:center;"> 5.616 </td>
#>    <td style="text-align:center;"> 0.000 </td>
#>    <td style="text-align:center;"> *** </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> BTheta(1) </td>
#>    <td style="text-align:center;"> -0.664 </td>
#>    <td style="text-align:center;"> 0.042 </td>
#>    <td style="text-align:center;"> -15.865 </td>
#>    <td style="text-align:center;"> 0.000 </td>
#>    <td style="text-align:center;"> *** </td>
#>   </tr>
#> </tbody>
#> <tfoot>
#> <tr><td style="padding: 0; " colspan="100%">
#> <sup></sup> <b>Signif. codes:</b> 0 '\*\*\*' 0.001 '\*\*' 0.01 '\*' 0.05 '.' 0.1 ' ' 1</td></tr>
#> <tr><td style="padding: 0; " colspan="100%">
#> <sup></sup> ARIMA (2,1,0)(0,1,1)</td></tr>
#> </tfoot>
#> </table>
#> 
#> <u><b>Regression model</b></u>
#> 
#> <table class="table" style="margin-left: auto; margin-right: auto;border-bottom: 0;">
#> <caption>Regression coefficients</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;">   </th>
#>    <th style="text-align:center;"> Coefficients </th>
#>    <th style="text-align:center;"> Std. Error </th>
#>    <th style="text-align:center;"> T-stat </th>
#>    <th style="text-align:center;"> P (&gt; | t|) </th>
#>    <th style="text-align:center;">  </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> Week days </td>
#>    <td style="text-align:center;"> 0.699 </td>
#>    <td style="text-align:center;"> 0.032 </td>
#>    <td style="text-align:center;"> 22.016 </td>
#>    <td style="text-align:center;"> 0.000 </td>
#>    <td style="text-align:center;"> *** </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Leap year </td>
#>    <td style="text-align:center;"> 2.323 </td>
#>    <td style="text-align:center;"> 0.690 </td>
#>    <td style="text-align:center;"> 3.367 </td>
#>    <td style="text-align:center;"> 0.001 </td>
#>    <td style="text-align:center;"> *** </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Easter [6] </td>
#>    <td style="text-align:center;"> -2.515 </td>
#>    <td style="text-align:center;"> 0.436 </td>
#>    <td style="text-align:center;"> -5.773 </td>
#>    <td style="text-align:center;"> 0.000 </td>
#>    <td style="text-align:center;"> *** </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> AO (5-2011) </td>
#>    <td style="text-align:center;"> 13.468 </td>
#>    <td style="text-align:center;"> 1.787 </td>
#>    <td style="text-align:center;"> 7.535 </td>
#>    <td style="text-align:center;"> 0.000 </td>
#>    <td style="text-align:center;"> *** </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> TC (4-2020) </td>
#>    <td style="text-align:center;"> -22.213 </td>
#>    <td style="text-align:center;"> 2.205 </td>
#>    <td style="text-align:center;"> -10.072 </td>
#>    <td style="text-align:center;"> 0.000 </td>
#>    <td style="text-align:center;"> *** </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> TC (3-2020) </td>
#>    <td style="text-align:center;"> -21.039 </td>
#>    <td style="text-align:center;"> 2.217 </td>
#>    <td style="text-align:center;"> -9.492 </td>
#>    <td style="text-align:center;"> 0.000 </td>
#>    <td style="text-align:center;"> *** </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> AO (5-2000) </td>
#>    <td style="text-align:center;"> 6.739 </td>
#>    <td style="text-align:center;"> 1.794 </td>
#>    <td style="text-align:center;"> 3.757 </td>
#>    <td style="text-align:center;"> 0.000 </td>
#>    <td style="text-align:center;"> *** </td>
#>   </tr>
#> </tbody>
#> <tfoot><tr><td style="padding: 0; " colspan="100%">
#> <sup></sup> <b>Signif. codes:</b> 0 '\*\*\*' 0.001 '\*\*' 0.01 '\*' 0.05 '.' 0.1 ' ' 1</td></tr></tfoot>
#> </table>
#> 
# }
```
