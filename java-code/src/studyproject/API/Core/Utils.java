package studyproject.API.Core;

import java.io.IOException;
import java.io.InputStream;

public class Utils {
	
	/**
     * reads the exact number of bytes specified from a InputStream to the specified buffer, no less
     * @param inStream the stream to read from
     * @param destination the byte array to copy the read bytes to
     * @param off offset for the byte array, all data gets copied in the array after this index
     * @param len the number of bytes to read and copy
     * @return the number of bytes read (=len)
     * @throws IOException
     */
	public static int readThisLength(InputStream inStream, byte[] destination, int off, int len) throws IOException{
		int readBytes = 0;
		if(off + len > destination.length){
			throw new IndexOutOfBoundsException("Can not read the specified length from the stream since the offset plus "
					+ "length is bigger than the provided array. Offset: " + off + ", len: " + len 
					+ ", arraysize: " + destination.length);
		}
		while(readBytes < len){
			readBytes = readBytes + inStream.read(destination, off + readBytes, len - readBytes);
		}
		return readBytes;
	}

}
