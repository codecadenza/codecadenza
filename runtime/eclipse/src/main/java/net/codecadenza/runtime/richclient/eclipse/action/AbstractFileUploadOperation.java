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
package net.codecadenza.runtime.richclient.eclipse.action;

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FILE_UPLOAD_OPERATION_MSG_ERR_UPLOAD;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FILE_UPLOAD_OPERATION_MSG_TITLE_UPLOAD;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FILE_UPLOAD_OPERATION_STATUS_PREPARE_UPLOAD;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;

import java.lang.invoke.MethodHandles;
import java.lang.reflect.InvocationTargetException;
import net.codecadenza.runtime.richclient.transport.event.DataProcessedEventController;
import net.codecadenza.runtime.richclient.transport.event.DataProcessedListener;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Display;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class for upload operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractFileUploadOperation implements IRunnableWithProgress, DataProcessedListener {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final String clientPath;
	private String serverPath;
	private IProgressMonitor monitor;

	/**
	 * Constructor
	 * @param clientPath
	 */
	protected AbstractFileUploadOperation(String clientPath) {
		this.clientPath = clientPath;
		DataProcessedEventController.addListener(this);
	}

	/**
	 * @param clientPath
	 * @return the fully qualified path name of the file on the server
	 * @throws Exception if the upload operation has failed
	 */
	protected abstract String uploadFile(String clientPath) throws Exception;

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.transport.event.DataProcessedListener#onPercentCompleted(double, long, long)
	 */
	@Override
	public final void onPercentCompleted(double percentage, long bytesProcessed, long totalBytesToProcess) {
		monitor.worked(1);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public final void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
		this.monitor = monitor;

		try {
			monitor.beginTask(getTranslation(FILE_UPLOAD_OPERATION_STATUS_PREPARE_UPLOAD), 100);

			logger.debug("Upload file '{}'", clientPath);

			// Upload a file
			serverPath = uploadFile(clientPath);

			monitor.done();

			logger.debug("Upload finished");
		}
		catch (final Exception e) {
			logger.error("Error while uploading file '{}'!", clientPath, e);

			final String title = getTranslation(FILE_UPLOAD_OPERATION_MSG_TITLE_UPLOAD);
			final String messageText = getTranslation(FILE_UPLOAD_OPERATION_MSG_ERR_UPLOAD) + e.getMessage();

			Display.getDefault()
					.syncExec(() -> MessageDialog.openInformation(Display.getCurrent().getActiveShell(), title, messageText));
		}
	}

	/**
	 * @return the fully qualified path name of the uploaded file
	 */
	public String getServerPath() {
		return serverPath;
	}

}
