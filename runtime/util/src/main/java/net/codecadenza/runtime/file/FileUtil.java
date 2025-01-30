/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.runtime.file;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import net.codecadenza.runtime.property.PropertyService;

/**
 * <p>
 * Utility class for file operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FileUtil {
	private static final String CURRENT_WORKING_DIRECTORY = System.getProperty("user.dir");
	private static final PropertyService propertyService = new PropertyService();

	/**
	 * Prevent instantiation
	 */
	private FileUtil() {

	}

	/**
	 * Copy a file and replace the target file if it already exists
	 * @param sourceFile
	 * @param targetFile
	 * @throws IOException if the copy operation has failed
	 */
	public static void copyFile(File sourceFile, File targetFile) throws IOException {
		Files.copy(sourceFile.toPath(), targetFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
	}

	/**
	 * Get a unique file name to the save file to a central repository
	 * @param groupName a token in order to distinguish between files that belong to different domain objects
	 * @return the generated file name
	 */
	public static synchronized String getUniqueFileName(String groupName) {
		String repositoryPath = propertyService.getStringProperty(PropertyService.PROP_REPOSITORY_FOLDER);
		final var fileName = groupName + "_" + System.nanoTime();

		if (repositoryPath.isEmpty())
			repositoryPath = CURRENT_WORKING_DIRECTORY;

		if (repositoryPath.endsWith(File.separator))
			repositoryPath += fileName;
		else
			repositoryPath += File.separator + fileName;

		return repositoryPath;
	}

	/**
	 * Get the contents of the file as a byte array
	 * @param file the file this method should read data from
	 * @return a byte array with the content of the file
	 * @throws IOException if the content could not be read
	 */
	public static byte[] getBytesFromFile(File file) throws IOException {
		// Get the size of the file
		final long length = file.length();

		// Create the byte array to hold the data
		final var bytes = new byte[(int) length];

		// Ensure that the file is not larger than Integer.MAX_VALUE!
		if (length > Integer.MAX_VALUE)
			throw new IOException("File " + file.getName() + " is too large!");

		try (final var is = new FileInputStream(file)) {
			// Fill the byte array
			int offset = 0;
			int numRead = 0;

			while ((offset < bytes.length) && ((numRead = is.read(bytes, offset, bytes.length - offset)) >= 0))
				offset += numRead;

			// Ensure that all the bytes have been read
			if (offset < bytes.length)
				throw new IOException("Could not completely read file " + file.getName());
		}

		return bytes;
	}

	/**
	 * Convert a byte[] into a Byte[]
	 * @param array
	 * @return the converted array
	 */
	public static Byte[] convertToByteArray(byte[] array) {
		final var returnArray = new Byte[array.length];

		for (int i = 0; i < array.length; i++)
			returnArray[i] = array[i];

		return returnArray;
	}

	/**
	 * Convert a given Byte[] into a byte[]
	 * @param array
	 * @return the converted array
	 */
	public static byte[] convertToByteArray(Byte[] array) {
		final var returnArray = new byte[array.length];

		for (int i = 0; i < array.length; i++)
			returnArray[i] = array[i];

		return returnArray;
	}

}
