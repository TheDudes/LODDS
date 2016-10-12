package studyproject.API.Lvl.Low;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.util.ArrayList;

import studyproject.API.Core.Utils;
import studyproject.API.Core.File.FileInfo;

/**
 * Class that handles sending the responses to
 * another client, the responses are formatted
 * according to the LODDS specification
 * @author Michael
 *
 */
public class Responses {
	
	private static final String UPDATE_FLAG = "upd ";
	private static final String ALL_FLAG = "all ";
	private static final String OK_FLAG = "OK\n";
	private static final int BUFFERSIZE = 4096;

	/**
	 * responds to the getInfoUp call and writes the timestamp and the fileInfos in
	 * the way that is specified in the specification to the provided outputstream
	 * 
	 * @param socketStream 
	 * 			the stream to write to
	 * 
	 * @param timestamp 
	 * 			the timestamp on which the fileInfo list was created
	 * 
	 * @param fileInfos 
	 * 			the list of fileInfos to send to the client
	 * 
	 * @return 
	 * 			0 or an error value
	 */
	public static int respondInfo(BufferedOutputStream socketStream, long timestamp, ArrayList<FileInfo> fileInfos) {
		try{
			if(timestamp == 0){
				socketStream.write((ALL_FLAG + timestamp + " " + fileInfos.size() + "\n").getBytes());
				socketStream.flush();
			} else {
				socketStream.write((UPDATE_FLAG + timestamp + " " + fileInfos.size() + "\n").getBytes());
				socketStream.flush();
			}
			for(FileInfo fileInfo: fileInfos){
				socketStream.write((fileInfo.fileAction.toString() + " " + fileInfo.checksum + " " + fileInfo.size
						+ " " + fileInfo.fileName + "\n").getBytes());
				socketStream.flush();
			}
		} catch(IOException e){
			return 1;
		}
		return 0;
	}

	/**
	 * responds to the getFile call and writes the contents of the file to which the fileStream is given
	 * to the stream. The function only writes the contents of the file between start and endIndex to the
	 * stream
	 * 
	 * @param socketStream 
	 * 			stream to write to
	 * 
	 * @param fileStream 
	 * 			fileStream from which to read from, still on index 0
	 * 
	 * @param startIndex 
	 * 			the index from which to start reading
	 * 
	 * @param endIndex 
	 * 			the index the function should stop reading
	 * 
	 * @return 
	 * 			0 or an error value
	 */
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
			Load.incrementLoad(endIndex - startIndex);
			while(sentBytes < endIndex - startIndex){
				if((endIndex - (currentPosition + startIndex)) > BUFFERSIZE){
					toRead = BUFFERSIZE;
				} else {
					toRead = (int)(endIndex - (currentPosition + startIndex));
				}
				Utils.readThisLength(fileStream, readBuffer, 0, toRead);
				//write the number of bytes we just read to the socketStream
				socketStream.write(readBuffer, 0, toRead);
				socketStream.flush();
				Load.decrementLoad(toRead);
				sentBytes += toRead;
			}
		} catch(IOException e){
			return 1;
		}
		return 0;
	}


	/**
	 * responds to the getSendPermission call by sending OK and then waiting to receive the bytes of
	 * the file that this method just gave permission to be sent
	 * 
	 * @param socket 
	 * 			socket to get the in and outputStream from
	 * 
	 * @param fileStream 
	 * 			the fileStream to the file where we write the file to
	 * 
	 * @param size 
	 * 			the size of the file we should write
	 * 
	 * @return 
	 * 			0 or an error value
	 */
	public static int respondSendPermission(Socket socket, FileOutputStream fileStream, long size) {
		try{
			long bytesRead = 0;
			int bytesToRead = 0;
			byte[] buffer = new byte[BUFFERSIZE];
			
			BufferedOutputStream outStream = new BufferedOutputStream(socket.getOutputStream());
			outStream.write(OK_FLAG.getBytes());
			outStream.close();
			
			BufferedInputStream inStream = new BufferedInputStream(socket.getInputStream());
			while(bytesRead < size){
				if((bytesRead + BUFFERSIZE) <= size){
					bytesToRead = BUFFERSIZE;
				} else{
					bytesToRead = (int)(size - bytesRead);
				}
				Utils.readThisLength(inStream, buffer, 0, bytesToRead);
				bytesRead += bytesToRead;
				fileStream.write(buffer, 0, bytesToRead);
			}
			inStream.close();
		} catch(IOException e){
			return 1;
		}
		return 0;
	}

}
