package studyproject.API.LowLvl;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;

import studyproject.API.Core.FileInfo;
import studyproject.API.Core.Utils;

public class Responses {
	
	private static final String UPDATE_FLAG = "upd ";
	private static final String ALL_FLAG = "all ";
	private static final String OK_FLAG = "OK";
	private static final int BUFFERSIZE = 4096;

	public static int respondInfoUp(BufferedOutputStream socketStream, long timestamp, ArrayList<FileInfo> fileInfos) {
		try{
			socketStream.write((UPDATE_FLAG + timestamp + " " + fileInfos.size() + "\n").getBytes());
			for(FileInfo fileInfo: fileInfos){
				socketStream.write((fileInfo.fileAction.toString() + " " + fileInfo.checksum + " " + fileInfo.size
						+ " " + fileInfo.fileName + "\n").getBytes());
			}
		} catch(IOException e){
			//TODO real error codes
			return -1;
		}
		return 0;
	}

	public static int respondInfoAll(BufferedOutputStream socketStream, long timestamp, ArrayList<FileInfo> fileInfos) {
		try{
			socketStream.write((ALL_FLAG + timestamp + " " + fileInfos.size() + "\n").getBytes());
			for(FileInfo fileInfo: fileInfos){
				socketStream.write((fileInfo.fileAction.toString() + " " + fileInfo.checksum + " " + fileInfo.size
						+ " " + fileInfo.fileName + "\n").getBytes());
			}
		} catch(IOException e){
			//TODO real error codes
			return -1;
		}
		return 0;
	}

	public static int respondFile(BufferedOutputStream socketStream, FileInputStream fileStream,
			long startIndex, long endIndex) {
		try{
			byte[] readBuffer = new byte[BUFFERSIZE];
			int toRead;
			long sentBytes = 0;
			long currentPosition = 0;
			//go to the starting index and skip all info until then
			while(currentPosition < startIndex){
				if((startIndex - currentPosition) > BUFFERSIZE){
					toRead = BUFFERSIZE;
				} else {
					toRead = (int)(startIndex - currentPosition);
				}
				currentPosition += fileStream.read(readBuffer, 0, toRead);
			}
			while(sentBytes < endIndex - startIndex){
				if((endIndex - (currentPosition + startIndex)) > BUFFERSIZE){
					toRead = BUFFERSIZE;
				} else {
					toRead = (int)(endIndex - (currentPosition + startIndex));
				}
				Utils.readThisLength(fileStream, readBuffer, 0, toRead);
				//write the number of bytes we just read to the socketStream
				socketStream.write(readBuffer, 0, toRead);
				sentBytes += toRead;
			}
		} catch(IOException e){
			//TODO real error codes
			return -1;
		}
		return 0;
	}

	public static int respondInfoLoad(BufferedOutputStream socketStream, long byteToSend) {
		try{
			socketStream.write((byteToSend + "\n").getBytes());
		} catch(IOException e){
			//TODO real error codes
			return -1;
		}
		return 0;
	}

	public static int respondSendPermission(BufferedOutputStream socketStream, FileInputStream fileStream, long size) {
		try{
			socketStream.write(OK_FLAG.getBytes());
		} catch(IOException e){
			//TODO real error codes
			return -1;
		}
		return 0;
	}

}
