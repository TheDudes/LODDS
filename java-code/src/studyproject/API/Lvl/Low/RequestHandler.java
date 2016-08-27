package studyproject.API.Lvl.Low;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import studyproject.API.Core.File.InfoList.InfoType;
import studyproject.API.Core.Request.GetFileRequest;
import studyproject.API.Core.Request.GetInfoRequest;
import studyproject.API.Core.Request.GetPermissionRequest;
import studyproject.API.Core.Request.Request;

public class RequestHandler {
	
	public static int parseRequest(InputStream socketStream, Request request){
		String currentLine;
		String[] lineParts;
		try(BufferedReader reader = new BufferedReader(new InputStreamReader(socketStream))){
			currentLine = reader.readLine();
			if(currentLine != null){
				lineParts = currentLine.split(" ");
				if(lineParts.length < 3){
					//TODO error codes
					return -4;
				}
				if(lineParts[0].equals("get")){
					switch (lineParts[1]) {
					case "info":
						GetInfoRequest getInfoRequest = new GetInfoRequest();
						getInfoRequest.timestamp = Long.parseLong(lineParts[2]);
						if(getInfoRequest.timestamp == 0){
							getInfoRequest.infoType = InfoType.all;
						} else{
							getInfoRequest.infoType = InfoType.upd;
						}
						break;
					case "file":
						if(lineParts.length != 5){
							//TODO return real error code
							return -2;
						}
						GetFileRequest getFileRequest = new GetFileRequest();
						getFileRequest.checksum = lineParts[2];
						getFileRequest.startIndex = Long.parseLong(lineParts[3]);
						getFileRequest.endIndex = Long.parseLong(lineParts[4]);
						break;
					case "send-permission":
						if(lineParts.length < 5){
							//TODO return real error code
							return -2;
						}
						GetPermissionRequest getPermissionRequest = new GetPermissionRequest();
						getPermissionRequest.fileSize = Long.parseLong(lineParts[2]);
						getPermissionRequest.timeout = Long.parseLong(lineParts[3]);
						String fileName = "";
						fileName += lineParts[4];
						for(int packetIndex = 5; packetIndex < lineParts.length; packetIndex++){
							fileName += " " + lineParts[packetIndex];
						}
						getPermissionRequest.fileName = fileName;
						break;
					default:
						break;
					}
				}
			}
		} catch(IOException e){
			//TODO return real error code
			return -1;
		} catch(NumberFormatException e){
			//TODO return real error code
			return -3;
		}
		return 0;
	}

}
