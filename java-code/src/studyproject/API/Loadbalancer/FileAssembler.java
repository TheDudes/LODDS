package studyproject.API.Loadbalancer;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Vector;
import java.util.logging.Level;

import studyproject.App;
import studyproject.API.Errors.ErrLog;
import studyproject.logging.APILvl;
import studyproject.logging.LogKey;

/**
 * class to assemble the temporary files created by the Loadbalancer into the
 * real file
 * 
 * @author Michael
 *
 */
public class FileAssembler extends Thread {

	private final int BUFFER_SIZE = 4096;
	private final long SLEEP_TIME;

	private String tmpFilePath, localPath;
	private byte[] buffer;
	// 0 = file transfer by Thread not done yet, 1 = fileTransfer of chunk is
	// done
	private long[] chunkStates;
	private long writtenBytes;
	private int readBytes;
	private int chunksTotal;
	private int lastWrittenChunk;

	/**
	 * 
	 * @param tmpFilePath
	 *            the location where all temporary files will be put
	 * @param localPath
	 *            the complete path where the final file will be put
	 * @param chunksTotal
	 *            the total number of parts the file will be split into
	 * @param chunkThreads
	 *            the vector with all the information about the threads that
	 *            pull the files
	 */
	public FileAssembler(String tmpFilePath, String localPath, int chunksTotal, Vector<ProgressInfo> chunkThreads) {
		lastWrittenChunk = -1;
		writtenBytes = 0;
		this.tmpFilePath = tmpFilePath;
		this.localPath = localPath;
		this.chunksTotal = chunksTotal;
		chunkStates = new long[this.chunksTotal];
		buffer = new byte[BUFFER_SIZE];
		SLEEP_TIME = Long.parseLong(App.properties.getProperty("fileAssemblerSleepTime"));
	}

	@Override
	public void run() {
		try (FileOutputStream writer = new FileOutputStream(new File(localPath), true)) {
			// as long as there are chunks that were not written
			while (lastWrittenChunk + 1 < chunksTotal) {
				if (isNextChunkReady()) {
					writtenBytes = 0;
					// open the tmp file with the number of the chunk, i.e. if
					// the chunk is 3 and the name of the file is /tmp/file then
					// the opened file is /tmp/file3
					try (FileInputStream reader = new FileInputStream(new File(tmpFilePath + (lastWrittenChunk + 1)))) {
						while (writtenBytes < chunkStates[lastWrittenChunk + 1]) {
							if (BUFFER_SIZE > chunkStates[lastWrittenChunk + 1] - writtenBytes) {
								readBytes = (int) (chunkStates[lastWrittenChunk + 1] - writtenBytes);
							} else {
								readBytes = BUFFER_SIZE;
							}
							reader.read(buffer, 0, readBytes);
							writer.write(buffer, 0, readBytes);
							writer.flush();
							writtenBytes += readBytes;
						}
					}
					File doneTmpFile = new File(tmpFilePath + (lastWrittenChunk + 1));
					if(!doneTmpFile.delete()){
						ErrLog.log(Level.INFO, LogKey.warning, APILvl.mid, "FileAssembler.run()",
								"could not delete tmp file with name " + tmpFilePath + (lastWrittenChunk + 1));
					}
					lastWrittenChunk++;
				} else {
					Thread.sleep(SLEEP_TIME);
				}
			}
		} catch (IOException e) {
			ErrLog.log(Level.SEVERE, LogKey.error, APILvl.mid, "FileAssembler.run()",
					"Encountered IOException while assembling a file: \n" + e.getMessage());
		} catch (InterruptedException e) {
			ErrLog.log(Level.SEVERE, LogKey.error, APILvl.mid, "FileAssembler.run()",
					"Encountered InterruptedException while assembling a file: \n" + e.getMessage());
		}
	}

	/**
	 * 
	 * @return if the next position in the array is not zero, which means that
	 *         the thread has finished downloading
	 */
	private boolean isNextChunkReady() {
		return chunkStates[lastWrittenChunk + 1] != 0;
	}

	/**
	 * 
	 * @param chunkNumber
	 *            the number of the chunk, starting from 0
	 * @param bytes
	 *            the number of bytes that were downloaded in this chunk
	 */
	public void setChunkReady(int chunkNumber, long bytes) {
		if (chunkNumber < chunksTotal) {
			chunkStates[chunkNumber] = bytes;
		}
	}

}
