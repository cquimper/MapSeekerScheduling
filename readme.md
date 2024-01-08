---------------------------------------------------------------------------------
# Instructions on how to perform the robustness test of schduling acquisition

## Data Preparation

>First step is to prepare the test tables. You can either download the dataset or generate a new one:
>1. If you decided to download the dataset, then:
>1.1 Go to **https://zenodo.org/records/8340001**, download the archive file **schedule_robustness_data.zip** and place it in the directory of the tool and unzip it. The test tables' files (e.g. **test_table_forest0_up_maxd_8.pl**) must appear at **data/model_seeker/data0** directory (replace the files if needed).
>1.2. From the same link, download the file **schedule_robustness_dimensions.pl** and place it in the **schedule_robustness** directory (replace the files if needed).
>2. If you want to generate the test tables anew, then run SICStus Prolog from the terminal (version 4.6.0 or a more recent version) and execute the following command:
>    `| ?- [gen_data_schedules], top, halt.`
> Update the file **tables_list.pl** if necessary.
> The process might take several days. It is possible to parallelize its execution. Check the file **gen_data_schedules.pl** for more details.



## Schedule Acquisition

>After the dataset is prepared the next step is to execute the shedule model acquisition. To acquire schedule models for each test table you need to perform three steps:
>1. Run SICStus Prolog from the terminal (version 4.6.0 or a more recent version) and execute the following command:
>   `| ?- [gen_metadata], top(1), halt.`
>2. Run SICStus Prolog from the terminal (version 4.6.0 or a more recent version) and execute the following command:
>   `| ?- [main], ms, halt.`
>3. Run SICStus Prolog from the terminal (version 4.6.0 or a more recent version) and execute the following command:
>   `| ?- [gen_resource_ctrs], top, halt.`




## Collection of the statistics

>After acquiring every schedule model you can collect the relevant statistics do:
>1. Copy file **found_model_1_1.pl** from the folder **data/model_seeker** to the folder **schedule_robustness** (replace the files if needed).
>2. Run SICStus Prolog from the terminal (version 4.6.0 or a more recent version) and execute the following command:
>   `[schedule_robustness_stats], create_stats, halt.`
>The results of this step will be collected in the file **schedule_robustness/stats.csv**
>3. Execute the python notebook **schedule_robustness/schedule robustness stats.ipynb** to print the final statistics (you may need to change the path to the file **schedule_robustness/stats.csv** in the code).




## Conversion to MiniZinc model

>Additionally, you can generate MiniZinc models for every acquired scheduling model and test each one individually. To do this, run SICStus Prolog from the terminal (version 4.6.0 or a more recent version) and execute the following command:
>   `[convert_to_mzn], top, halt.`
>The model files for each table will be stored in the **data/model_seeker/data0** directory.