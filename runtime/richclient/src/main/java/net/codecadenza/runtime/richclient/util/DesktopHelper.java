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
package net.codecadenza.runtime.richclient.util;

import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.DESKTOP_HELPER_ERR_ILLEGAL_ARGUMENT;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.DESKTOP_HELPER_ERR_OS_NOT_SUPPORTED;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.getTranslation;

import java.awt.Desktop;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.net.URI;
import java.net.URISyntaxException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Helper class for desktop operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DesktopHelper {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String MAIL_TO = "mailto:";
	private static final String HTTP = "http://";
	private static final String HTTPS = "https://";
	private static final String WWW = "www.";

	private String link;
	private File file;
	private String tempFileName;
	private String tempFileSuffix;

	/**
	 * Constructor
	 * @param link
	 */
	public DesktopHelper(String link) {
		this.link = link;
	}

	/**
	 * Constructor
	 * @param file
	 */
	public DesktopHelper(File file) {
		this.file = file;
	}

	/**
	 * Constructor
	 * @param tempFileName
	 * @param tempFileSuffix
	 */
	public DesktopHelper(String tempFileName, String tempFileSuffix) {
		this.tempFileName = tempFileName;
		this.tempFileSuffix = tempFileSuffix;
	}

	/**
	 * Initialize a {@link Desktop} instance
	 * @return the desktop object
	 * @throws IllegalStateException if the use of the {@link Desktop} class is not supported on the current platform
	 */
	private Desktop init() {
		if (!Desktop.isDesktopSupported())
			throw new IllegalStateException(getTranslation(DESKTOP_HELPER_ERR_OS_NOT_SUPPORTED));

		return Desktop.getDesktop();
	}

	/**
	 * Open a file with the default application
	 * @throws IllegalStateException if the use of the {@link Desktop} class is not supported on the current platform
	 * @throws IOException if the file could not be opened
	 */
	public void openFile() throws IOException {
		final Desktop desktop = init();

		logger.debug("Open file {}", file.getName());

		desktop.open(file);
	}

	/**
	 * Write the content to the given file or create a temporary file and open it with the default application
	 * @param content
	 * @throws IllegalStateException if the use of the {@link Desktop} class is not supported on the current platform
	 * @throws IOException if either the content could not be written to a temporary file, or if this file could not be opened
	 */
	public void openFile(String content) throws IOException {
		if (file == null)
			file = File.createTempFile(tempFileName, tempFileSuffix);

		if (content != null)
			try (final var fw = new FileWriter(file)) {
				fw.write(content);
			}

		openFile();
	}

	/**
	 * Open the browser with the given link
	 * @throws IllegalArgumentException if the link is either null or empty
	 * @throws IllegalStateException if the use of the {@link Desktop} class is not supported on the current platform
	 * @throws IOException if the link could not be opened in the default browser
	 * @throws URISyntaxException if the link doesn't represent a valid URI
	 */
	public void openWebLink() throws IOException, URISyntaxException {
		final Desktop desktop = init();

		if (link == null || link.isEmpty())
			throw new IllegalArgumentException(getTranslation(DESKTOP_HELPER_ERR_ILLEGAL_ARGUMENT));

		if (link.startsWith(HTTP) || link.startsWith(HTTPS)) {
			desktop.browse(new URI(link));
			return;
		}

		if (link.startsWith(WWW)) {
			desktop.browse(new URI(HTTP + link));
			return;
		}

		desktop.browse(new URI(HTTP + WWW + link));
	}

	/**
	 * Open the default email client with the given address
	 * @throws IllegalArgumentException if the link is either null or empty
	 * @throws IllegalStateException if the use of the {@link Desktop} class is not supported on the current platform
	 * @throws IOException if the link could not be opened in the default mail application
	 * @throws URISyntaxException if the link doesn't represent a valid URI
	 */
	public void openEmailLink() throws IOException, URISyntaxException {
		final Desktop desktop = init();

		if (link == null || link.isEmpty())
			throw new IllegalArgumentException(getTranslation(DESKTOP_HELPER_ERR_ILLEGAL_ARGUMENT));

		if (link.startsWith(MAIL_TO)) {
			desktop.mail(new URI(link));
			return;
		}

		desktop.mail(new URI(MAIL_TO + link));
	}

}
