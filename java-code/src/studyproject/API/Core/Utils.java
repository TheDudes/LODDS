package studyproject.API.Core;

import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

public class Utils {

	/**
	 * reads the exact number of bytes specified from a InputStream to the
	 * specified buffer, no less
	 * 
	 * @param inStream
	 *            the stream to read from
	 * 
	 * @param destination
	 *            the byte array to copy the read bytes to
	 * 
	 * @param off
	 *            offset for the byte array, all data gets copied in the array
	 *            after this index
	 * 
	 * @param len
	 *            the number of bytes to read and copy
	 * 
	 * @return the number of bytes read (=len)
	 * 
	 * @throws IOException
	 */

	public static int readThisLength(InputStream inStream, byte[] destination, int off, int len)
			throws IOException, IndexOutOfBoundsException {
		int readBytes = 0;
		if (off + len > destination.length) {
			throw new IndexOutOfBoundsException(
					"Can not read the specified length from the stream since the offset plus "
							+ "length is bigger than the provided array. Offset: " + off + ", len: " + len
							+ ", arraysize: " + destination.length);
		}
		while (readBytes < len) {
			readBytes = readBytes + inStream.read(destination, off + readBytes, len - readBytes);
		}
		return readBytes;
	}

	/**
	 * 
	 * @param source
	 *            the array containing the wanted bytes
	 * @param startIndex
	 *            the first index, included in the returned byte array
	 * @param endIndex
	 *            the last index, included in the returned byte array
	 * @return a byte array containing the specified bytes
	 */
	public static byte[] getBytesFromTo(byte[] source, int startIndex, int endIndex) {
		byte[] returnValue = new byte[endIndex - startIndex];
		for (int index = 0; index < endIndex - startIndex; index++) {
			returnValue[index] = source[index + startIndex];
		}
		return returnValue;
	}

	/**
	 * This function formats a given unix timestamp to the format "HH:mm:ss a"
	 * and returns it as String
	 * 
	 * @param unixSeconds
	 *            The Unix Timestamp you want to format
	 * @return the formatted unix timestamp as string e.g. "10:30:41 PM"
	 */
	public static String formatUnixTimestamp(long unixSeconds) {
		// *1000 is to convert seconds to milliseconds
		Date date = new Date(unixSeconds * 1000L);
		SimpleDateFormat sdf = new SimpleDateFormat("HH:mm:ss a");
		// give a timezone reference
		sdf.setTimeZone(TimeZone.getDefault());

		return sdf.format(date);

	}

}
