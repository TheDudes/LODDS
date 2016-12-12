package studyproject.API.Lvl.Low;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.regex.Pattern;

import studyproject.API.Core.File.InfoList.InfoType;
import studyproject.API.Core.Request.GetFileRequest;
import studyproject.API.Core.Request.GetInfoRequest;
import studyproject.API.Core.Request.GetPermissionRequest;
import studyproject.API.Core.Request.RequestContainer;

/**
 * Class to parse an incoming request and extract the passed information
 * Checking if the request matches a format from the specification is done via
 * regular expressions
 * 
 * @author Michael
 *
 */
public class RequestHandler {

	private static final String GET_INFO_REGEX = "get info \\d{1,19}";
	private static final String GET_FILE_REGEX = "get file \\w{40} \\d{1,19} \\d{1,19}";
	private static final String GET_SEND_PERMISSION_REGEX = "get send-permission \\w{40} \\d{1, 19} \\d{1,19} .*";

	/**
	 * Reads the next line from the InputStream, matches it with regular
	 * expressions and puts the contained information in the Request object if
	 * the line is matching a valid line from the specification
	 * 
	 * @param socketStream
	 *            the stream to read from
	 * 
	 * @param request
	 *            the empty request which will be filled
	 * 
	 * @return 0 or an error code
	 */
	public static int parseRequest(InputStream socketStream, RequestContainer request) {
		String currentLine;
		try {
			BufferedReader reader = new BufferedReader(new InputStreamReader(socketStream));
			currentLine = reader.readLine();
			if (currentLine != null) {
				if (Pattern.matches(GET_INFO_REGEX, currentLine)) {
					return parseGetInfo(currentLine, request);
				} else if (Pattern.matches(GET_FILE_REGEX, currentLine)) {
					return parseGetFile(currentLine, request);
				} else if (Pattern.matches(GET_SEND_PERMISSION_REGEX, currentLine)) {
					return parseGetSendPermission(currentLine, request);
				} else {
					return 2;
				}
			} else {
				return 2;
			}
		} catch (IOException e) {
			return 1;
		} catch (NumberFormatException e) {
			return 2;
		}
	}

	/**
	 * extracts the information and puts it in a GetInfoRequest
	 * 
	 * @param currentLine
	 *            the read line
	 * 
	 * @param requestContainer
	 *            the container to put the extracted information into
	 * 
	 * @return 0
	 * 
	 * @throws NumberFormatException
	 *             if the number can not be parsed
	 */
	private static int parseGetInfo(String currentLine, RequestContainer requestContainer)
			throws NumberFormatException {
		String[] lineParts;
		lineParts = currentLine.split(" ");
		GetInfoRequest getInfoRequest = new GetInfoRequest();
		getInfoRequest.timestamp = Long.parseLong(lineParts[2]);
		if (getInfoRequest.timestamp == 0) {
			getInfoRequest.infoType = InfoType.all;
		} else {
			getInfoRequest.infoType = InfoType.upd;
		}
		requestContainer.request = getInfoRequest;
		return 0;
	}

	/**
	 * extracts the information and puts it in a GetFileRequest
	 * 
	 * @param currentLine
	 *            the read line
	 * 
	 * @param requestContainer
	 *            the container to put the extracted information into
	 * 
	 * @return 0
	 * 
	 * @throws NumberFormatException
	 *             if the number can not be parsed
	 */
	private static int parseGetFile(String currentLine, RequestContainer requestContainer)
			throws NumberFormatException {
		String[] lineParts;
		lineParts = currentLine.split(" ");
		GetFileRequest getFileRequest = new GetFileRequest();
		getFileRequest.checksum = lineParts[2];
		getFileRequest.startIndex = Long.parseLong(lineParts[3]);
		getFileRequest.endIndex = Long.parseLong(lineParts[4]);
		requestContainer.request = getFileRequest;
		return 0;
	}

	/**
	 * extracts the information and puts it in a GetPermissionRequest
	 * 
	 * @param currentLine
	 *            the read line
	 * 
	 * @param requestContainer
	 *            the container to put the extracted information into
	 * 
	 * @return 0
	 * 
	 * @throws NumberFormatException
	 *             if the number can not be parsed
	 */
	private static int parseGetSendPermission(String currentLine, RequestContainer requestContainer)
			throws NumberFormatException {
		String[] lineParts;
		lineParts = currentLine.split(" ");
		GetPermissionRequest getPermissionRequest = new GetPermissionRequest();
		getPermissionRequest.checksum = lineParts[1];
		getPermissionRequest.fileSize = Long.parseLong(lineParts[2]);
		getPermissionRequest.timeout = Long.parseLong(lineParts[3]);
		String fileName = "";
		fileName += lineParts[4];
		for (int packetIndex = 5; packetIndex < lineParts.length; packetIndex++) {
			fileName += " " + lineParts[packetIndex];
		}
		getPermissionRequest.fileName = fileName;
		requestContainer.request = getPermissionRequest;
		return 0;
	}

}
