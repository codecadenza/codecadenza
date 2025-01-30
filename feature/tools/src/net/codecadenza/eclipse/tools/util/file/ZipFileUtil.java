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
package net.codecadenza.eclipse.tools.util.file;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ui.dialogs.IOverwriteQuery;
import org.eclipse.ui.wizards.datatransfer.ImportOperation;
import org.eclipse.ui.wizards.datatransfer.ZipFileStructureProvider;

/**
 * <p>
 * Utility class for zip files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ZipFileUtil {
	/**
	 * Prevent instantiation
	 */
	private ZipFileUtil() {

	}

	/**
	 * Read all files from the given zip file and copy them to the destination path
	 * @param srcZipFile
	 * @param destPath
	 * @param monitor
	 * @return a list with all file names that are included in the zip file
	 * @throws InvocationTargetException if the import operation has failed
	 * @throws InterruptedException if the import operation has been interrupted
	 */
	public static List<String> importFilesFromZip(ZipFile srcZipFile, IPath destPath, IProgressMonitor monitor)
			throws InvocationTargetException, InterruptedException {
		final var zipFiles = new ArrayList<String>();
		final Enumeration<?> zipEntries = srcZipFile.entries();
		final var structureProvider = new ZipFileStructureProvider(srcZipFile);

		while (zipEntries.hasMoreElements()) {
			final var entry = (ZipEntry) zipEntries.nextElement();
			zipFiles.add(entry.getName());
		}

		new ImportOperation(destPath, structureProvider.getRoot(), structureProvider, file -> IOverwriteQuery.YES).run(monitor);

		return zipFiles;
	}

	/**
	 * Unzip the provided {@link InputStream} into the given destination directory
	 * @param stream
	 * @param destPath
	 * @throws IOException if the unzip operation has failed
	 */
	public static void unzip(InputStream stream, Path destPath) throws IOException {
		final File destDir = destPath.toFile();

		if (!destDir.exists())
			destDir.mkdirs();

		final var buffer = new byte[1024];

		try (final var zis = new ZipInputStream(stream)) {
			ZipEntry zipEntry;

			while ((zipEntry = zis.getNextEntry()) != null) {
				final var newFile = new File(destDir, zipEntry.getName());

				try (final var fos = new FileOutputStream(newFile)) {
					int len;

					while ((len = zis.read(buffer)) > 0)
						fos.write(buffer, 0, len);
				}

				zis.closeEntry();
			}
		}
	}

}
