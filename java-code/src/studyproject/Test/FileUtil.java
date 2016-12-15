package studyproject.Test;

import static org.junit.Assert.fail;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

/**
 * class to create and remove files for the Junit tests
 * 
 * @author Michael
 *
 */
public class FileUtil {

	public static final String dir = "test";
	public static final String sourceFile = "sourceFile";
	public static final String destFile = "destFile";
	public static final String fileContents = "Lorem ipsum dolor sit amet, consectetur adipisici elit, sed"
			+ " eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim"
			+ " veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi"
			+ " consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu"
			+ " fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, sunt in"
			+ " culpa qui officia deserunt mollit anim id est laborum.";
	public static final String fileContents2 = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy "
			+ "eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam "
			+ "et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor "
			+ "sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt "
			+ "ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et "
			+ "ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum "
			+ "dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore "
			+ "magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet "
			+ "clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.\n"
			+ "Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum "
			+ "dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent "
			+ "luptatum zzril delenit augue duis dolore te feugait nulla facilisi. Lorem ipsum dolor sit amet, consectetuer "
			+ "adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.\n"
			+ "Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea "
			+ "commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, "
			+ "vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit"
			+ " praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi.\n"
			+ "Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet doming id quod mazim placerat "
			+ "facer possim assum. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh "
			+ "euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis "
			+ "nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat.\n"
			+ "Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum "
			+ "dolore eu feugiat nulla facilisis.\n"
			+ "At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata "
			+ "sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, "
			+ "sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua."
			+ " At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea "
			+ "takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing "
			+ "elitr, At accusam aliquyam diam diam dolore dolores duo eirmod eos erat, et nonumy sed tempor et et "
			+ "invidunt justo labore Stet clita ea et gubergren, kasd magna no rebum. sanctus sea sed takimata ut "
			+ "vero voluptua. est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing "
			+ "elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat.\n"
			+ "Consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam "
			+ "erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd "
			+ "gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, "
			+ "consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna "
			+ "aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet "
			+ "clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor "
			+ "sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore "
			+ "magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. "
			+ "Stet clita kasd gubergren, no sea takimata sanctus.\n"
			+ "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt "
			+ "ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo "
			+ "dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor "
			+ "sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor "
			+ "invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et "
			+ "justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem "
			+ "ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy "
			+ "eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos "
			+ "et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus "
			+ "est Lorem ipsum dolor sit amet.\n"
			+ "Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, "
			+ "vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim "
			+ "qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi. Lorem "
			+ "ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut "
			+ "laoreet dolore magna aliquam erat volutpat.\n"
			+ "Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut "
			+ "aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate v"
			+ "elit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et "
			+ "accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis "
			+ "dolore te feugait nulla facilisi.\n"
			+ "Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet doming id quod "
			+ "mazim placerat facer possim assum. Lorem ipsum dolor sit amet, consectetuer adipiscing e"
			+ "lit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. "
			+ "Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl "
			+ "ut aliquip ex ea commodo\n";

	private static ArrayList<String> filesCreated = new ArrayList<String>();
	private static ArrayList<String> dirsCreated = new ArrayList<String>();

	/**
	 * create a file for testing
	 * 
	 * @param toWrite
	 *            the fileContents to write
	 * @param fileName
	 *            the name of the file
	 */
	public static void createFile(String toWrite, String fileName) {
		createFile(toWrite, fileName, true);
	}

	/**
	 * create a file for testing with the default fileName
	 * 
	 * @param toWrite
	 *            the fileContents to write
	 */
	public static void createFile(String toWrite) {
		createFile(toWrite, sourceFile);
	}

	/**
	 * create a file for testing
	 * 
	 * @param toWrite
	 *            the fileContents to write
	 * @param fileName
	 *            the name of the file
	 * @param addToCleanup
	 *            add the file to the files that get cleaned up when the
	 *            cleanUp() method is called
	 */
	public static void createFile(String toWrite, String fileName, boolean addToCleanup) {
		File directory = new File(dir);
		directory.mkdir();
		if (fileName.charAt(fileName.length() - 1) != '/' && fileName.contains("/")) {
			File file = new File(dir + "/" + fileName.substring(0, fileName.lastIndexOf('/')));
			if (!file.exists()) {
				file.mkdirs();
				dirsCreated.add(dir + "/" + fileName.substring(0, fileName.lastIndexOf('/')));
			}
		}
		try (BufferedWriter writer = new BufferedWriter(new FileWriter(dir + "/" + fileName))) {
			writer.write(toWrite);
		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}
		if (addToCleanup) {
			filesCreated.add(fileName);
		}
	}

	public static void addToCleanUp(String fileName) {
		filesCreated.add(fileName);
	}

	public static boolean deleteFile(String fileName) {
		File file = new File(dir + "/" + fileName);
		if (!file.delete()) {
			System.err.println("could not delete file " + dir + "/" + fileName);
			return false;
		}
		filesCreated.remove(fileName);
		return true;
	}

	/**
	 * delete the created files
	 */
	public static void cleanUp() {
		File file;
		int size = filesCreated.size(), index = size - 1;
		for (int counter = 0; counter < size; counter++) {
			file = new File(dir + "/" + filesCreated.get(index));
			if (!file.delete()) {
				System.err.println("could not delete file " + dir + "/" + filesCreated.get(index));
			} else {
				filesCreated.remove(index);
			}
			index--;
		}
		size = dirsCreated.size();
		index = size - 1;
		for (int counter = 0; counter < size; counter++) {
			file = new File(dirsCreated.get(index));
			if (!file.delete()) {
				System.err.println("could not delete dir " + dirsCreated.get(index));
			} else {
				dirsCreated.remove(index);
			}
			index--;
		}
		file = new File(dir);
		if (!file.delete()) {
			System.err.println("could not delete directory " + dir);
		}
	}

}
