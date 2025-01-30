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
package net.codecadenza.runtime.richclient.swing.file;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FILE_UPLOAD_OPERATION_BYTES_PROCESSED;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FILE_UPLOAD_OPERATION_MSG_UPLOAD_FINISHED;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FILE_UPLOAD_OPERATION_STATUS_PREPARE_UPLOAD;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.io.File;
import java.util.ArrayList;
import javax.swing.SwingUtilities;
import net.codecadenza.runtime.file.ByteConverter;
import net.codecadenza.runtime.richclient.swing.widget.StatusReceivable;
import net.codecadenza.runtime.richclient.transport.ServiceLocator;
import net.codecadenza.runtime.richclient.transport.event.DataProcessedEventController;
import net.codecadenza.runtime.richclient.transport.event.DataProcessedListener;

/**
 * <p>
 * File upload operation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FileUploadOperation implements DataProcessedListener {
	private final StatusReceivable status;

	/**
	 * Constructor
	 * @param status
	 */
	public FileUploadOperation(StatusReceivable status) {
		this.status = status;

		// Register this object to be a listener for appropriate events!
		DataProcessedEventController.addListener(this);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.transport.event.DataProcessedListener#onPercentCompleted(double, long, long)
	 */
	@Override
	public final void onPercentCompleted(final double percentage, final long bytesProcessed, final long totalBytesToProcess) {
		SwingUtilities.invokeLater(() -> {
			final var params = new ArrayList<>();
			params.add(ByteConverter.convert(bytesProcessed));
			params.add((int) percentage);

			status.setStatusInfoMessage(getTranslation(FILE_UPLOAD_OPERATION_BYTES_PROCESSED, params.toArray()));
		});
	}

	/**
	 * @param clientPath
	 * @return the absolute path of the file on the server
	 * @throws IllegalStateException if the service locator has not been initialized
	 * @throws IllegalArgumentException if the given file either doesn't exist, or if it is a directory
	 * @throws Exception if the upload operation has failed
	 */
	public String startUpload(final String clientPath) throws Exception {
		SwingUtilities.invokeLater(() -> status.setStatusInfoMessage(getTranslation(FILE_UPLOAD_OPERATION_STATUS_PREPARE_UPLOAD)));

		// Upload the file
		final String path = ServiceLocator.uploadFile(new File(clientPath));

		SwingUtilities.invokeLater(() -> status.setStatusInfoMessage(getTranslation(FILE_UPLOAD_OPERATION_MSG_UPLOAD_FINISHED)));

		DataProcessedEventController.removeListener(FileUploadOperation.this);

		return path;
	}

}
