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
package net.codecadenza.runtime.richclient.transport.util;

import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.DATA_DOWNLOAD_THREAD_ERR_FILE_CREATION;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.DATA_DOWNLOAD_THREAD_ERR_NO_FILE;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.DATA_DOWNLOAD_THREAD_ERR_NO_HTTP_CON;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.getTranslation;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.invoke.MethodHandles;
import java.net.HttpURLConnection;
import javax.crypto.Cipher;
import javax.crypto.CipherInputStream;
import net.codecadenza.runtime.crypto.CipherFactory;
import net.codecadenza.runtime.property.PropertyService;
import net.codecadenza.runtime.richclient.transport.event.DataProcessedEventController;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Thread to download data from a HTTP stream into a file
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DataDownloadThread extends Thread {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	public static final int DEFAULT_BUFFER_SIZE = 1024;
	private static final String SECRET = new PropertyService().getStringProperty(PropertyService.PROP_TRANSPORT_SECRET);

	private final int bufferSize;
	private Cipher decryptionCipher;
	private final long totalBytesToProcess;
	private InputStream downloadStream;
	private final File outputFile;
	private final boolean decryptInput;
	private boolean stopDownload;

	/**
	 * Constructor
	 * @param connection
	 * @param path
	 * @param decryptInput
	 * @param bufferSize
	 * @throws Exception if the cipher couldn't be initialized
	 */
	public DataDownloadThread(HttpURLConnection connection, String path, boolean decryptInput, int bufferSize) throws Exception {
		if (connection == null)
			throw new IllegalStateException(getTranslation(DATA_DOWNLOAD_THREAD_ERR_NO_HTTP_CON));

		// Check if the target file exists and create it if necessary
		outputFile = new File(path);

		if (outputFile.isDirectory())
			throw new IllegalStateException(getTranslation(DATA_DOWNLOAD_THREAD_ERR_NO_FILE));

		if (!outputFile.exists()) {
			final boolean success = outputFile.createNewFile();

			if (!success)
				throw new IllegalStateException(getTranslation(DATA_DOWNLOAD_THREAD_ERR_FILE_CREATION) + outputFile.getName());
		}

		this.bufferSize = bufferSize;
		this.totalBytesToProcess = connection.getContentLength();
		this.downloadStream = connection.getInputStream();
		this.decryptInput = decryptInput;

		if (decryptInput)
			this.decryptionCipher = new CipherFactory(SECRET).getDecryptionCipher();
	}

	/**
	 * Constructor
	 * @param connection
	 * @param path
	 * @param decryptInput
	 * @throws Exception if the cipher couldn't be initialized
	 */
	public DataDownloadThread(HttpURLConnection connection, String path, boolean decryptInput) throws Exception {
		this(connection, path, decryptInput, DEFAULT_BUFFER_SIZE);
	}

	/**
	 * Stop the download operation
	 */
	public void stopDownload() {
		stopDownload = true;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Thread#run()
	 */
	@Override
	public void run() {
		super.run();

		final var buf = new byte[bufferSize];
		long bytesProcessed = 0;
		double percentage = 0;
		double lastPercentageSent = 0;
		int numRead = 0;

		try (final var fout = new FileOutputStream(outputFile)) {
			if (decryptInput)
				downloadStream = new CipherInputStream(downloadStream, decryptionCipher);

			while ((numRead = downloadStream.read(buf)) >= 0 && !stopDownload) {
				// We save the number of the bytes that have been downloaded so far
				bytesProcessed += numRead;

				percentage = (bytesProcessed * 100) / (double) totalBytesToProcess;

				// We shouldn't fire an event for every single chunk!
				if (percentage >= (lastPercentageSent + 1)) {
					lastPercentageSent = percentage;
					DataProcessedEventController.fireEvent(percentage, bytesProcessed, totalBytesToProcess);
				}

				fout.write(buf, 0, numRead);
			}
		}
		catch (final Exception e) {
			logger.error("Error while performing download operation!", e);

			// We have to handle all checked exceptions and simply throw a runtime exception if something goes wrong!
			throw new RuntimeException(e.getMessage());
		}
		finally {
			try {
				if (downloadStream != null)
					downloadStream.close();
			}
			catch (final IOException e) {
				logger.warn("Could not close input stream!", e);
			}
		}
	}

}
