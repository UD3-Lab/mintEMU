import fitz
import os
import sys

# source: https://towardsdatascience.com/read-a-multi-column-pdf-using-pymupdf-in-python-4b48972f82dc

def check_file_path(file_path):

    '''
    Function returns a list of files from the directory chosen by the user and to be converted. Checks if the provided file path exists and whether it holds the adequate file formats (pdf).

    Parameters
    -----------
    file_path : str
        The provided file path

    Returns
    ------------
    file_list: list
        The list of files (together with path) chosen by the user and to be converted

    '''

    file_list =  []
    #If the path provided is a single file 
    if os.path.isfile(file_path):       
        # If it 
        if file_path.endswith('pdf'):
            # Retrieve the file_name of the path
            file_name = os.path.basename(file_path)
            file_list = [file_path]
        else: 
            sys.exit("The format of the file: " + os.path.splitext(file_path)[1] +" does not match the selected requested format: pdf")
    else:
        assert os.path.isdir(file_path) , "Invalid directory"
        
        for file in os.listdir(file_path):
            if file.endswith("pdf"):
                file_list.append( os.path.join(file_path, file))

        assert len(file_list)>0 , "No files in the directory match the selected collection format: pdf"

        input_string = ''
        for file in file_list:
            input_string += '\n'+  '- ' + os.path.basename(file)  

        print('The files selected in upload: ' + input_string + '\n')

        return(file_list)
        
def convert_pdf(file_list, start_pages, end_pages):
    '''
    Function converts the pdf files to text. It allow unusual pdf layout (multiple columns)

    Parameters
    -----------
    file_list : list 
        The list of files to be converted
        
    start_pages : list 
        The list of start pages to be used to subset PDF pages
        
    end_pages : list 
        The list of end pages to be used to subset PDF pages

    Returns
    ------------
    text_list: list 
        The list  body of text of each of the file as character strings 

    '''
    text_list = []
    for i, file in enumerate(file_list): 
        l = []
        assert os.path.isfile(file), f"No such file: {file}"
        with fitz.open(file) as doc:
            for page in doc:
                text = page.get_text()
                l.append(text)
        start_page = int(start_pages[i]) - 1
        if not int(end_pages[i]) == 0:
          end_page = int(end_pages[i])
          l = l[start_page:end_page]
        else:
          l = l[start_page:]
        l = ' '.join(l)
        text_list.append(l)
    
    return(text_list)






