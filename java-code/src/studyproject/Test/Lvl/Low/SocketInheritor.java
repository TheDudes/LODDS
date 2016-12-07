package studyproject.Test.Lvl.Low;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

public class SocketInheritor extends Socket {

	private String readWritePath;
	private String readPath;
	private FileOutputStream outputStream;
	private FileInputStream inputStream;
	
	public SocketInheritor(String readWritePath, String readPath) {
		this.readWritePath = readWritePath;
		this.readPath = readPath;
	}
	
	public OutputStream getOutputStream() {
		try {
			outputStream = new FileOutputStream(readWritePath);
			return outputStream;
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	public InputStream getInputStream() {
		try {
			inputStream = new FileInputStream(readPath);
			return inputStream;
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public synchronized void close() throws IOException {
		if(inputStream != null){
			inputStream.close();
		}
		if(outputStream != null){
			outputStream.close();
		}
		super.close();
	}
}
