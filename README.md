Documentation is currently hosted at:

- https://seandavi.github.io/SRAdbV2/

# The SRAdbV2 Package

The Sequence Read Archive (SRA) is NIH's primary archive of
high-throughput sequencing data and is part of the International
Nucleotide Sequence Database Collaboration (INSDC) that includes at
the NCBI Sequence Read Archive (SRA), the European Bioinformatics
Institute (EBI), and the DNA Database of Japan (DDBJ). Data submitted
to any of the three organizations are shared among them.

This package serves as a resource for searching and large-scale
processing of SRA metadata. This is a complete re-imagining of the 
SRAdb package which, while still quite useful, requires a large 
download and suffers from difficulties in maintainability. The SRAdbV2
package provides an R client to a high-performance web-based 
API (usable outside of R if needed).

The data served by the SRAdbV2 package are processed in the following steps. 

- Parse SRA XML files into json format (nearly lossless)
- Use Apache Spark to transform the data 
- Load the data from Apache Spark into an ElasticSearch 
  backend
- Provide access to the data via a serverless, swagger-based API

The raw API is documented and queriable here:

- https://api-omicidx.cancerdatasci.org/sra/1.0/ui/

# Installation

```{r}
install.packages('BiocManager')
BiocManager::install('seandavi/SRAdbV2')
```

# Usage

See the vignette for details of usage.


