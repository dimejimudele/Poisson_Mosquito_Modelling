
"""
This function outputs a csv of time series features extracted from raster files (climatic variable and field epidemiology data)
"""



def prepare_data(main_folder, dummy_value, training_data_file, test_data_file):


    """

    Arguments:
    --------------------------
    main_folder: folder containing all the Earth observation data and rasterized mosquito
                    weekly mosquito data. Files should be arrannged in order of weeks.
                    The folder must contain a file named 'mosquitraps.tif' which ia a raster file 
                    of the mosquitrap locations and identification number, and subfolders namely: 
                    urban and rural. Each of these subfolders must contain yet another subfolders 
                    namely day_lst, night_lst, precipitation, ndvi, ndwi
    
    dummy_value: value given to dummy images used to fill weeks for which data is absent for
                    some of the variables. e.g since the MODIS LST is 16day temporal resolution
                    as against the MODIS NDVI and NDWI data which is 8 days,
                    it is necessary to create a dummy image with one value (the dummy value)
                    to fill these weeks.

    training_data_file: filename to save output training data

    testing_data_file: filename to save output testing data
    """

    """
    Import dependencies
    """
    import numpy as np
    import numpy as np
    import pandas as pd 
    from osgeo import gdal
    import numpy as np
    from os import listdir
    from os.path import isfile, join
    from osgeo import osr
    import matplotlib.pyplot as plt

    #Mosquitrap Location
    mosquitrap_data = main_folder + '\\mosquitraps.tif'

    """
    List files in folders
    """
    mosquito_dir = main_folder + '\\mosquito_data\\'
 

    ndvi_u_dir = main_folder + '\\urban\\ndvi\\'
    ndvi_r_dir = main_folder + '\\rural\\ndvi\\'

    ndwi_u_dir = main_folder + '\\urban\\ndwi\\'
    ndwi_r_dir = main_folder + '//rural//ndwi//'

    prec_u_dir = main_folder + '\\urban\\precipitation\\'
    prec_r_dir = main_folder + '\\rural\\precipitation\\'

    day_lst_u_dir = main_folder + '\\urban\\day_lst\\'
    night_lst_u_dir = main_folder + '\\urban\\night_lst\\'
    day_lst_r_dir = main_folder + '\\rural\\day_lst\\'
    night_lst_r_dir = main_folder + '\\rural\\night_lst\\'

    print(listdir(mosquito_dir))
    """
    List EO data filesfiles
    """
    mosquito_list = [mosquito_dir + f for f in listdir(mosquito_dir) if isfile(join(mosquito_dir, f))]


    """
    List EO data filesfiles
    """
    ndvi_u_list = [ndvi_u_dir  + f for f in listdir(ndvi_u_dir) if isfile(join(ndvi_u_dir, f))]
    ndvi_r_list = [ndvi_r_dir  + f for f in listdir(ndvi_r_dir) if isfile(join(ndvi_r_dir, f))]

    ndwi_u_list = [ndwi_u_dir  + f for f in listdir(ndwi_u_dir) if isfile(join(ndwi_u_dir, f))]
    ndwi_r_list = [ndwi_r_dir  + f for f in listdir(ndwi_r_dir) if isfile(join(ndwi_r_dir, f))]
  
    prec_u_list = [prec_u_dir  + f for f in listdir(prec_u_dir) if isfile(join(prec_u_dir, f))]
    prec_r_list = [prec_r_dir  + f for f in listdir(prec_r_dir) if isfile(join(prec_r_dir, f))]


    day_lst_u_list = [day_lst_u_dir  + f for f in listdir(day_lst_u_dir) if isfile(join(day_lst_u_dir, f))]
    day_lst_r_list = [day_lst_r_dir  + f for f in listdir(day_lst_r_dir) if isfile(join(day_lst_r_dir, f))]

    night_lst_u_list = [night_lst_u_dir  + f for f in listdir(night_lst_u_dir) if isfile(join(night_lst_u_dir, f))]
    night_lst_r_list = [night_lst_r_dir  + f for f in listdir(night_lst_r_dir) if isfile(join(night_lst_r_dir, f))]

    assert len(ndvi_r_list) == len(ndwi_r_list) == len(prec_r_list) == len(day_lst_r_list) == len(night_lst_r_list),\
    'Number of rural zone images must be equal for all climatic variables'

    assert len(ndvi_u_list) == len(ndwi_u_list) == len(prec_u_list) == len(day_lst_u_list) == len(night_lst_u_list),\
    'Number of urban zone images must be equal for all climatic variables'

    assert len(ndvi_u_list) == len(ndvi_r_list), 'Number of urban and rual zone images must be equal'
    

    ndvi_u_train = []
    ndvi_r_train = []

    ndwi_u_train = []
    ndwi_r_train = []

    prec_u_train = []
    prec_r_train = []

    day_lst_u_train = []
    day_lst_r_train = []

    night_lst_u_train = []
    night_lst_r_train = []

    mosquito_pop_train = []

    ndvi_u_test = []
    ndvi_r_test = []

    ndwi_u_test = []
    ndwi_r_test = []

    prec_u_test = []
    prec_r_test = []

    day_lst_u_test = []
    day_lst_r_test = []

    night_lst_u_test = []
    night_lst_r_test = []

    mosquito_pop_test = []

    mosquito_pop_test = []
    """
    Mosquitrap location data processes
    """
    mosquitrap_raster = gdal.Open(mosquitrap_data)
    mosquitrap_array = mosquitrap_raster.ReadAsArray().astype(np.float)
    mosquitrap_array[mosquitrap_array == -1] = np.nan 

    mosquitraps_id = [int(x) for x in (np.unique(mosquitrap_array)) if ~np.isnan(x)]

     #Select 50% training mosquitraps
    msk_mosquitraps_id = (np.random.rand(len(mosquitraps_id)) <= 0.5).tolist()
    np.random.seed(100)
    train_mosquitraps = [mosquitraps_id[x] for x in range(len(mosquitraps_id)) if msk_mosquitraps_id[x] == True]
    test_mosquitraps = [mosquitraps_id[x] for x in range(len(mosquitraps_id)) if msk_mosquitraps_id[x] == False]

    print(mosquito_list)
    """
    Read images using gdal
    """

    for i in range(len(ndvi_u_list)):
        mosquito_raster = gdal.Open(mosquito_list[i])

        ndvi_u_img = gdal.Open(ndvi_u_list[i])
        ndvi_r_img = gdal.Open(ndvi_r_list[i])

        ndwi_u_img = gdal.Open(ndwi_u_list[i])
        ndwi_r_img = gdal.Open(ndwi_r_list[i])

        prec_u_img = gdal.Open(prec_u_list[i])
        prec_r_img = gdal.Open(prec_r_list[i])

        day_lst_u_img = gdal.Open(day_lst_u_list[i])
        day_lst_r_img = gdal.Open(day_lst_r_list[i])
        night_lst_u_img = gdal.Open(night_lst_u_list[i])
        night_lst_r_img = gdal.Open(night_lst_r_list[i])



        """
        Read images as numpy arrays
        """

        mosquito_array = mosquito_raster.ReadAsArray().astype(np.float)

        ndvi_u_array = ndvi_u_img.ReadAsArray().astype(np.float)
        ndvi_r_array = ndvi_r_img.ReadAsArray().astype(np.float)

        ndwi_u_array = ndwi_u_img.ReadAsArray().astype(np.float)
        ndwi_r_array = ndwi_r_img.ReadAsArray().astype(np.float)

        prec_u_array = prec_u_img.ReadAsArray().astype(np.float)
        prec_r_array = prec_r_img.ReadAsArray().astype(np.float)

        day_lst_u_array =  day_lst_u_img.ReadAsArray().astype(np.float)
        day_lst_r_array =  day_lst_r_img.ReadAsArray().astype(np.float)

        night_lst_u_array = night_lst_u_img.ReadAsArray().astype(np.float)
        night_lst_r_array = night_lst_r_img.ReadAsArray().astype(np.float)


        #Remote no data values (Set to -1) from the mosquito population and mosquitrap location data
        mosquito_array[mosquito_array == -1] = np.nan 
        
        mosquito_pop_alltrain = []
        for idtrain in train_mosquitraps:
            a,b = np.where(mosquitrap_array == idtrain)
            mosquito_pop_alltrain.append(mosquito_array[a,b][0])
        
        mosquito_train_sum = np.sum(mosquito_pop_alltrain)
        mosquito_pop_train.append(mosquito_train_sum)

        mask = np.random.choice([False, True], ndvi_r_array.shape , p =[0.5, 0.5])

        """
        Extract EO data features
        """

        mask = np.random.choice([False, True], ndvi_u_array.shape , p =[0.5, 0.5])

        a, b = np.where(mask == True)
        _ndvi_u_train = np.mean(ndvi_u_array[a,b])
        _ndwi_u_train = np.mean(ndwi_u_array[a,b])
        _day_lst_u_train = np.mean(day_lst_u_array[a,b])
        _night_lst_u_train = np.mean(night_lst_u_array[a,b])

        _ndvi_r_train = np.mean(ndvi_r_array[a,b])
        _ndwi_r_train = np.mean(ndwi_r_array[a,b])
        _day_lst_r_train = np.mean(day_lst_r_array[a,b])
        _night_lst_r_train = np.mean(night_lst_r_array[a,b])

        ndvi_u_train.append(_ndvi_u_train)
        ndvi_r_train.append(_ndvi_r_train)

        ndwi_u_train.append(_ndwi_u_train)
        ndwi_r_train.append(_ndwi_r_train)

        day_lst_u_train.append(_day_lst_u_train)
        day_lst_r_train.append(_day_lst_r_train)

        night_lst_u_train.append(_night_lst_u_train)
        night_lst_r_train.append(_night_lst_r_train)

        prec_u_train.append(np.max(prec_u_array))
        prec_r_train.append(np.max(prec_r_array))

        mosquito_pop_alltest = []
        for idtest in test_mosquitraps:
            a,b = np.where(mosquitrap_array == idtest)
            mosquito_pop_alltest.append(mosquito_array[a,b][0])
        
        mosquito_test_sum = np.sum(mosquito_pop_alltest)
        mosquito_pop_test.append(mosquito_test_sum)

        a, b = np.where(mask != True)
        _ndvi_u_test = np.mean(ndvi_u_array[a,b])
        _ndwi_u_test = np.mean(ndwi_u_array[a,b])
        _day_lst_u_test = np.mean(day_lst_u_array[a,b])
        _night_lst_u_test = np.mean(night_lst_u_array[a,b])

        _ndvi_r_test = np.mean(ndvi_r_array[a,b])
        _ndwi_r_test = np.mean(ndwi_r_array[a,b])
        _day_lst_r_test = np.mean(day_lst_r_array[a,b])
        _night_lst_r_test = np.mean(night_lst_r_array[a,b])

        ndvi_u_test.append(_ndvi_u_test)
        ndvi_r_test.append(_ndvi_r_test)

        ndwi_u_test.append(_ndwi_u_test)
        ndwi_r_test.append(_ndwi_r_test)

        day_lst_u_test.append(_day_lst_u_test)
        day_lst_r_test.append(_day_lst_r_test)

        night_lst_u_test.append(_night_lst_u_test)
        night_lst_r_test.append(_night_lst_r_test)

        prec_u_test.append(np.max(prec_u_array))
        prec_r_test.append(np.max(prec_r_array))

    df_train = pd.DataFrame({
        'y': mosquito_pop_train,
        'ndvi_u': ndvi_u_train,
        'ndvi_r': ndvi_r_train,
        'ndwi_u': ndwi_u_train,
        'ndwi_r': ndwi_r_train,
        'tempd_u': day_lst_u_train,
        'tempd_r': day_lst_r_train,
        'tempn_u': night_lst_u_train,
        'tempn_r': night_lst_r_train,
        'prec_u': prec_u_train,
        'prec_r': prec_r_train
    })

    df_test = pd.DataFrame({
        'y': mosquito_pop_test,
        'ndvi_u': ndvi_u_test,
        'ndvi_r': ndvi_r_test,
        'ndwi_u': ndwi_u_test,
        'ndwi_r': ndwi_r_test,
        'tempd_u': day_lst_u_test,
        'tempd_r': day_lst_r_test,
        'tempn_u': night_lst_u_test,
        'tempn_r': night_lst_r_test,
        'prec_u': prec_u_test,
        'prec_r': prec_r_test
    })

    df_test.iloc[df_test== dummy_value] =np.nan
    df_train.iloc[df_train== dummy_value] =np.nan

    df_train_interpolate = df_test.astype(float).interpolate(method='spline', order = 3,  limit_direction = 'forward')
    df_train_interpolate = df_train.astype(float).interpolate(method='spline', order = 3, limit_direction = 'forward')

    df_train_interpolate.to_csv(training_data_file)
    df_train_interpolate.to_csv(test_data_file)


#Test

"""
main_folder = ('\\output')
dummy_value = -99999
training_data_file = ('\\output\\training.csv')

test_data_file = ('\\output\\test.csv')
dummy_value = -1
prepare_data(main_folder, dummy_value, training_data_file, test_data_file)
"""