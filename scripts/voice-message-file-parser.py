#!/bin/env python3 

#für die Umwandlung
import pydub
#für die Erkennung
import speech_recognition
#für den remove der datei und cwd
import os

data_path = os.getcwd() + "/"
fileslist = os.listdir()
failed = "failed"
success = "success"

for file in fileslist:
    path_to_file = data_path + file
    path_to_wav = data_path + "audio.wav"
    lang = 'de-DE'

    if file[-4:] == '.ogg':
    #Convert
        fileextenstion = file[-3:]
        sound = pydub.AudioSegment.from_ogg(
            path_to_file).export(path_to_wav, format="wav")
    elif file[-4:] == '.aac':
        fileextenstion = file[-3:]
        sound = pydub.AudioSegment.from_file(
            path_to_file, fileextenstion).export(path_to_wav, format="wav")
#    elif file[-5:] == '.opus':
#        sound = pydub.AudioSegment.from_file(
#            path_to_file, file[-4:]).export(path_to_wav, format="wav")
    else:
        continue

    #Recognizer
    recognizer = speech_recognition.Recognizer()
    google_audio = speech_recognition.AudioFile(
        path_to_wav)

    with google_audio as source:
        audio = recognizer.record(source)

    try:
        text = recognizer.recognize_google(audio, language=lang)
    except:
        print(file)
        print("some error happen")
        print("")
        if not os.path.isdir(data_path + failed + "_" + fileextenstion ):
            os.mkdir(failed + "_" + fileextenstion)
        os.rename(path_to_file, data_path + failed + "_" + fileextenstion + "/" + file)
        continue

    #Removefile
    #os.remove(path_to_wav)
    print(file)
    print("<Erkannter Text>: {}".format(text))
    print("")
    if not os.path.isdir(data_path + success + "_" + fileextenstion ):
        os.mkdir(success + "_" + fileextenstion)
    os.rename(path_to_file, data_path + success + "_" + fileextenstion + "/" + file)
