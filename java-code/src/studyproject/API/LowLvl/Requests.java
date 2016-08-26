package studyproject.API.LowLvl;

import java.io.BufferedOutputStream;
import java.io.IOException;

public class Requests {
	
	private static final String GET_INFO_UP = "get info up ";
	private static final String GET_INFO_LOAD = "get info load\n";
	private static final String GET_FILE = "get file ";
	private static final String GET_SEND_PERMISSION = "get send-permission ";

	public static int getInfoUp(BufferedOutputStream socketStream, long timestamp) {
		try{
			socketStream.write((GET_INFO_UP + timestamp + "\n").getBytes());
		} catch(IOException e){
			//TODO insert real error code
			return -1;
		}
		return 0;
	}

	public static int getFile(BufferedOutputStream socketStream, String checksum, long startIndex, long endIndex) {
		try{
			socketStream.write((GET_FILE + checksum + " " + startIndex + " " + endIndex + "\n").getBytes());
		} catch(IOException e){
			//TODO insert real error code
			return -1;
		}
		return 0;
	}

	public static int getInfoLoad(BufferedOutputStream socketStream) {
		try{
			socketStream.write((GET_INFO_LOAD).getBytes());
		} catch(IOException e){
			//TODO insert real error code
			return -1;
		}
		return 0;
	}

	public static int getSendPermission(BufferedOutputStream socketStream, long size, String fileName, long timeout) {
		try{
			socketStream.write((GET_SEND_PERMISSION + size + " " + fileName + " " + timeout + "\n").getBytes());
		} catch(IOException e){
			//TODO insert real error code
			return -1;
		}
		return 0;
	}

}
