package studyproject.API.Lvl.Low;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.regex.Pattern;

import studyproject.API.Core.Timestamp;
import studyproject.API.Core.Utils;
import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.File.InfoList.FileInfoListType;
import studyproject.API.Core.File.InfoList.InfoType;

public class Handles {

	private static final int BUFFERSIZE = 4096;
	private static final int TIMEOUT = 10000; // Timeout in milliseconds
	private static final int TIMEINTERVAL = 1000; // Timeoutinterval
	private static final String GET_INFO_HEAD_REGEX = "(upd|all) \\d{1,19} \\d{1,19}";
	private static final String GET_INFO_BODY_LINE_REGEX = "(add|del) \\w{64} \\d{1,19} [^/\\\\:*?\"<>|%]*";

	/**
	 * Handles incoming info responses to the getInfoUp request
	 * 
	 * @param socketStream
	 *            BufferedReader to read from
	 * @param fileInfos
	 *            ArrayList with information to the changed files
	 * @param timestamp
	 *            Timestamp object holding the timestamp of the last requester
	 *            filesystem update
	 * @param infoType
	 *            FileInfoListType object containing the info type
	 * @return integer representing the result. Negative value if function fails
	 */
	public static int handleInfo(BufferedReader socketStream, ArrayList<FileInfo> fileInfos, Timestamp timestamp,
			FileInfoListType infoType) {
		FileInfo fileInfo;
		String currentLine;
		String[] params;
		try {
			currentLine = socketStream.readLine();
			if (!Pattern.matches(GET_INFO_HEAD_REGEX, currentLine)) {
				return 2;
			}
			params = currentLine.split(" ");
			if (params[0].equals(InfoType.upd.toString())) {
				infoType.type = InfoType.upd;
			} else {
				infoType.type = InfoType.all;
			}
			timestamp.value = Long.valueOf(params[1]);
			for (int i = 0; i < Integer.valueOf(params[2]); i++) {
				currentLine = socketStream.readLine();
				if (!Pattern.matches(GET_INFO_BODY_LINE_REGEX, currentLine)) {
					return 2;
				}
				params = currentLine.split(" ");
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
	 * 
	 * @param socketStream
	 *            the BufferedInputStream to read from
	 * @param fileStream
	 *            the FileOutputStream to write read bytes to
	 * @param size
	 *            in bytes to read from BufferedInputStream
	 * @return integer representing the result. Negative value if function fails
	 */
	public static int handleFile(BufferedInputStream socketStream, FileOutputStream fileStream, long size) {
		int readSize;
		try {
			byte[] byteArray = new byte[BUFFERSIZE];
			while (size > 0) {
				if(size < byteArray.length){
					readSize = Utils.readThisLength(socketStream, byteArray, 0, (int)size);
				} else {
					readSize = Utils.readThisLength(socketStream, byteArray, 0, byteArray.length);
				}
				fileStream.write(byteArray, 0, readSize);
				size -= readSize;
			}
		} catch (IOException e) {
			return -2;
		}
		return 0;
	}

	/**
	 * Check if permission to send is permitted
	 * 
	 * @param socketStream
	 *            the BufferedReader to read from
	 * @return integer representing the result. Negative value if function fails
	 */
	public static int handleSendPermission(BufferedReader socketStream) {
		String s;
		long endTime = System.currentTimeMillis() + TIMEOUT;
		while (System.currentTimeMillis() < endTime) {
			try {
				if ((s = socketStream.readLine()) != null) {
					Thread.sleep(TIMEINTERVAL);
					if (!s.equals("OK")) {
						return -1;
					}
					continue;
				}
				break;
			} catch (IOException e) {
				return -2;
			} catch (InterruptedException e) {
				return -3;
			}
		}
		return 0;
	}
}
