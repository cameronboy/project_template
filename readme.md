### Project Template

Goal is to house a general starting-point structure that can be modified by project, if necessary,
to fit the needs of the project.


This template has the following base, structure:



```
project_template
|------data
|      |---external  <- Third party data
|      |---interim   <- Transformed intermediate data, not ready for modelling
|      |---processed <- Prepared data, ready for modelling
|      |---raw       <- Immutable original data
|------models        <- Serialized model
|------notebooks     <- Jupyter notebooks for exploration, communication and protyping
|------src           <- Folder containing project source code
       |---data      <- Folder containing scripts to download/generate data
       |---features  <- Folder containing scripts to transform data for modelling
       |---model     <- Folder containing scripts to train and predict
```
