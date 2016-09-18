package studyproject.API.Lvl.Mid;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.Socket;

import studyproject.API.Lvl.Low.Handles;
import studyproject.API.Lvl.Low.Requests;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class FileConnectionThread extends Thread implements Runnable{

	private UserInfo user;
	private String checksum;
	private String localPath;
	private long size;
	private int startIndex = 0;
	private int endIndex = 0;
	
	public FileConnectionThread(UserInfo user, String checksum, long size, String localPath){
		this.user = user;
		this.checksum = checksum;
		this.localPath = localPath;
		this.size = size;
	}
	
	public FileConnectionThread(UserInfo user, String checksum, long size, String localPath, int startIndex, int endIndex){
		this.user = user;
		this.checksum = checksum;
		this.localPath = localPath;
		this.startIndex = startIndex;
		this.endIndex = endIndex;
		this.size = size;
	}
	
	@Override
	public void run() {
		int returnValue;
		try(Socket socket = new Socket(user.getIpAddress(), user.getPort());
				BufferedOutputStream outStream = new BufferedOutputStream(socket.getOutputStream());
				BufferedInputStream inStream = new BufferedInputStream(socket.getInputStream());
				FileOutputStream fileOutStream = new FileOutputStream(new File(localPath))){
			if(endIndex == 0){
				returnValue = Requests.getFile(outStream, checksum, startIndex, size - startIndex);
			} else{
				returnValue = Requests.getFile(outStream, checksum, startIndex, endIndex);
			}
			if(returnValue != 0){
				//TODO error handling
			}
			returnValue = Handles.handleFile(inStream, fileOutStream, endIndex - startIndex);
			if(returnValue != 0){
				//TODO error handling
			}
		} catch(IOException e){
			//TODO error handling
		}
	}

	

}
