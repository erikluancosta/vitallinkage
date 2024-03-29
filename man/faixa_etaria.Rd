% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/faixa_etaria.R
\name{faixa_etaria}
\alias{faixa_etaria}
\title{Categoriza a idade em grupos específicos}
\usage{
faixa_etaria(df)
}
\arguments{
\item{df}{Um data frame contendo a coluna 'nu_idade_anos' representando idades em anos.}
}
\value{
O data frame de entrada com uma coluna adicional 'faixa_etaria' representando os grupos etários.
}
\description{
Esta função categoriza a idade em grupos específicos.
}
\examples{
\dontrun{
df <- data.frame(nu_idade_anos = c(4, 15, 27, 35, 50, 65, 80))
resultado <- faixa_etaria(df)
}

}
