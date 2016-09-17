package studyproject.Test.Lvl.Low;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

public class SocketInheritor extends Socket {

	String readWritePath;
	String readPath;
	
	public SocketInheritor(String readWritePath, String readPath) {
		this.readWritePath = readWritePath;
		this.readPath = readPath;
	}
	
	public OutputStream getOutputStream() {
		try {
			return new FileOutputStream(readWritePath);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	public InputStream getInputStream() {
		try {
			return new FileInputStream(readPath);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		return null;
	}
}
