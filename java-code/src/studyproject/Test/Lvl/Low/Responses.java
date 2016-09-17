package studyproject.Test.Lvl.Low;

import java.io.*;
import java.util.ArrayList;
import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;

public class Responses {
	
	public static void main(String[] args) {
		try {
			BufferedOutputStream socketStream = new BufferedOutputStream(new FileOutputStream("C:/Users/Chris/Documents/temp.txt"));
			long timestamp = System.currentTimeMillis();
			ArrayList<FileInfo> fileInfos = new ArrayList<FileInfo>();
			
			BufferedReader inputStream = new BufferedReader(new FileReader("C:/Users/Chris/Documents/temp.txt"));
			String str;
			
			fileInfos.add(new FileInfo("000001", 1000001, "asdf1.txt", FileAction.add));
			fileInfos.add(new FileInfo("000002", 1000002, "asdf2.txt", FileAction.del));
			fileInfos.add(new FileInfo("000003", 1000003, "asdf3.txt", FileAction.add));
			
			System.out.println("*** respondInfo called with timestamp being zero");
			if (studyproject.API.Lvl.Low.Responses.respondInfo(socketStream, 0, fileInfos) == 0) {
				while((str = inputStream.readLine()) != null) {
					System.out.println(str);
				}
				System.out.println("reached end of file");
			} else {
				System.out.println("respondInfo failed");
			}

			System.out.println("*** respondInfo called with timestamp being NOT zero");
			if (studyproject.API.Lvl.Low.Responses.respondInfo(socketStream, timestamp, fileInfos) == 0) {
				while((str = inputStream.readLine()) != null) {
					System.out.println(str);
				}
				System.out.println("reached end of file");
			} else {
				System.out.println("respondInfo failed");
			}
			
			socketStream.close();
			inputStream.close();
			
		} catch(Exception e) {
			System.out.println(e.getMessage());
		}
	}
}
