package studyproject.API.Lvl.Low;

import java.io.*;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import studyproject.API.Core.Timestamp;
import studyproject.API.Core.Utils;
import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileHasher;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.File.InfoList.FileInfoListType;
import studyproject.API.Core.File.InfoList.InfoType;
import studyproject.API.Errors.ErrorFactory;
import studyproject.logging.LogKey;

public class Handles {

	private static final int BUFFERSIZE = 4096;
	private static final String GET_INFO_HEAD_REGEX = "(upd|all) \\d{1,19} \\d{1,19}";
	private static final String GET_INFO_BODY_LINE_REGEX = "(add|del) " + FileHasher.getHashRegex()
			+ " \\d{1,19} [^\\\\:*?\"<>|%]*";

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
		String filePath;
		String[] params;
		int numberOfLines;
		try {
			currentLine = socketStream.readLine();
			Logger.getGlobal().log(ErrorFactory.build(Level.INFO, LogKey.info, currentLine));
			if (!Pattern.matches(GET_INFO_HEAD_REGEX, currentLine)) {
				Logger.getGlobal().log(ErrorFactory.build(Level.WARNING, LogKey.warning,
						"Pattern does not match. Return value not equals zero"));
				return 2;
			}
			params = currentLine.split(" ");
			if (params[0].equals(InfoType.upd.toString())) {
				infoType.type = InfoType.upd;
			} else {
				infoType.type = InfoType.all;
			}
			timestamp.value = Long.valueOf(params[1]);
			numberOfLines = Integer.parseInt(params[2]);
			for (int i = 0; i < Integer.valueOf(numberOfLines); i++) {
				currentLine = socketStream.readLine();
				Logger.getGlobal().log(ErrorFactory.build(Level.INFO, LogKey.info, currentLine));

				if (!Pattern.matches(GET_INFO_BODY_LINE_REGEX, currentLine)) {
					Logger.getGlobal().log(ErrorFactory.build(Level.WARNING, LogKey.warning,
							"Pattern does not match. Return value not equals zero"));
					return 2;
				}
				params = currentLine.split(" ");

				FileAction fileInfoFileAction;

				if (params[0].equals(FileAction.add.toString())) {
					fileInfoFileAction = FileAction.add;
				} else {
					fileInfoFileAction = FileAction.del;
				}
				filePath = currentLine.substring(currentLine.indexOf("/"));
				fileInfo = new FileInfo(params[1], Long.valueOf(params[2]), filePath, "", fileInfoFileAction);
				fileInfos.add(fileInfo);
			}
		} catch (IOException e) {
			Logger.getGlobal().log(ErrorFactory.build(Level.SEVERE, LogKey.error, "IOException thrown: ", e));
			return 1;
		} catch (NumberFormatException e) {
			Logger.getGlobal().log(ErrorFactory.build(Level.SEVERE, LogKey.error, "NumberFormatException thrown: " + e));
			return 2;
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
				if (size < byteArray.length) {
					readSize = Utils.readThisLength(socketStream, byteArray, 0, (int) size);
				} else {
					readSize = Utils.readThisLength(socketStream, byteArray, 0, byteArray.length);
				}
				fileStream.write(byteArray, 0, readSize);
				size -= readSize;
			}
		} catch (IOException e) {
			return 1;
		}
		return 0;
	}

	/**
	 * Check if permission to send is permitted
	 *
	 * @param socket
	 *            the socket to read the permission from
	 * @param timeout
	 *            the time in ms until the request times out
	 * @return integer representing the result. Negative value if function fails
	 */
	public static int handleSendPermission(Socket socket, long timeout) {
		String readLine = "";
		try {
			BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
			socket.setSoTimeout((int) timeout);
			readLine = reader.readLine();
			if (readLine.equals("OK")) {
				return 0;
			} else {
				Logger.getGlobal().log(ErrorFactory.build(Level.INFO, LogKey.respondSendPermission,
						"handleSendPermission failed. Return negative value"));
				return 5;
			}
		} catch (SocketTimeoutException s) {
			Logger.getGlobal().log(ErrorFactory.build(Level.SEVERE, LogKey.error, "SocketTimeoutException thrown: ", s));
			return 3;
		} catch (IOException e) {
			Logger.getGlobal().log(ErrorFactory.build(Level.SEVERE, LogKey.error, "IOException thrown: ", e));
			return 1;
		}
	}
}
