package studyproject.API.LowLvl;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import studyproject.API.Core.FileAction;
import studyproject.API.Core.FileInfo;
import studyproject.API.Core.Timestamp;
import studyproject.API.Core.Utils;
import studyproject.API.Core.FileInfoList.FileInfoListType;
import studyproject.API.Core.FileInfoList.InfoType;

public class Handles {
	
	private static final int BUFFERSIZE = 4096;

	/**
	 * Handles incoming info responses to the getInfoUp request
	 * @param socketStream BufferedReader to read from
	 * @param fileInfos ArrayList with information to the changed files
	 * @param timestamp Timestamp object holding the timestamp of the last requester filesystem update
	 * @param infoType FileInfoListType object containing the info type 
	 * @return integer representing the result. Negative value if function fails
	 */
	public static int handleInfoUp(BufferedReader socketStream, ArrayList<FileInfo> fileInfos, Timestamp timestamp, FileInfoListType infoType) {
		FileInfo fileInfo;
		String[] params;
		try {
			params = socketStream.readLine().split(" ");
			if (params[0].equals("upd")) {
				infoType.type = InfoType.upd;
			} else {
				infoType.type = InfoType.all;
			}
			timestamp.value = Long.valueOf(params[1]);
			for (int i = 0; i < Integer.valueOf(params[2]); i++) {
				params = socketStream.readLine().split(" ");
				fileInfo = new FileInfo();
				if (params[0].equals(FileAction.add.toString())) {
					fileInfo.fileAction = FileAction.add;
				} else {
					fileInfo.fileAction = FileAction.del;
				}
				fileInfo.checksum = params[1];
				fileInfo.size = Long.valueOf(params[2]);
				fileInfo.fileName = params[3];
				fileInfos.add(fileInfo);
			}
		} catch (IOException e) {
			return -1;
		} catch (NumberFormatException e) {
			return -2;
		}
		return 0;
	}

	/**
	 * Writes incoming files from a BufferedInputStream to an output file
	 * @param socketStream the BufferedInputStream to read from
	 * @param fileStream the FileOutputStream to write read bytes to
	 * @param size in bytes to read from BufferedInputStream
	 * @return integer representing the result. Negative value if function fails
	 */
	public static int handleFile(BufferedInputStream socketStream, FileOutputStream fileStream, long size) {
		try {
			byte[] byteArray = new byte[BUFFERSIZE];
			while(size > 0) {
				int readSize = Utils.readThisLength(socketStream, byteArray, 0, byteArray.length);
				fileStream.write(byteArray);
				size -= readSize;
			}
		} catch (IOException e) {
			return -2;
		}
		return 0;
	}

	/**
	 * Handles the response to the getInfoLoad request and filters the answer
	 * @param socketStream the BufferedReader to read from
	 * @return the number outstanding bytes to send. Negative value if fails
	 */
	public static long handleInfoLoad(BufferedReader socketStream) {
		long byteToSend;
		try {
			byteToSend =  Long.valueOf(socketStream.readLine());
		} catch (NumberFormatException e) {
			return -1;
		} catch (IOException e) {
			return -2;
		}
		return byteToSend;
	}

	/**
	 * Check if permission to send is permitted
	 * @param socketStream the BufferedReader to read from
	 * @return integer representing the result. Negative value if function fails
	 */
	public static int handleSendPermission(BufferedReader socketStream) {
		try {
			String s = socketStream.readLine();
			if (!s.equals("OK")) {
				return -1;
			}
		} catch (IOException e) {
			return -2;
		}
		return 0;
	}
}
