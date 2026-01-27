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
package net.codecadenza.runtime.webclient.vaadin.dialog;

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_CANCEL;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.FILE_UPLOAD_DIALOG_TITLE;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_UPLOAD_ERROR;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_UPLOAD_ILLEGAL_MIMETYPE;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_UPLOAD_TITLE;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.ModalityMode;
import com.vaadin.flow.component.Unit;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.upload.Upload;
import com.vaadin.flow.server.streams.UploadHandler;
import java.io.File;
import java.io.Serializable;
import java.lang.invoke.MethodHandles;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Dialog for uploading a single file to the server. The dialog creates a temporary file on the server that the consumer can use
 * as soon as the upload operation has been finished successfully!
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FileUploadDialog extends AbstractTitleDialog {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = 838665681410915830L;

	private final Set<String> acceptedFileTypes = new HashSet<>();
	private Integer maxFileSize;
	private Upload upload;
	private UploadFinishedListener uploadFinishedListener;

	public interface UploadFinishedListener extends Serializable {
		/**
		 * Callback method the notifies the receiver that the selected file has been uploaded successfully!
		 * @param file
		 * @param originalFileName
		 * @throws Exception if the operation has failed
		 */
		void onUploadFinished(File file, String originalFileName) throws Exception;
	}

	/**
	 * Constructor
	 * @param title
	 * @param maxFileSize
	 * @param locale
	 */
	public FileUploadDialog(String title, Integer maxFileSize, Locale locale) {
		super(title, locale);

		this.maxFileSize = maxFileSize;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.dialog.AbstractTitleDialog#onAttach(com.vaadin.flow.component.AttachEvent)
	 */
	@Override
	protected void onAttach(AttachEvent attachEvent) {
		super.onAttach(attachEvent);

		setModality(ModalityMode.STRICT);
		setWidth(400, Unit.PIXELS);
		setHeight(250, Unit.PIXELS);

		final var uploadHandler = UploadHandler.toTempFile((metadata, file) -> onUploadSucceeded(file, metadata.fileName()));

		upload = new Upload(uploadHandler);
		upload.setMaxFiles(1);
		upload.setId("upload");
		upload.setAcceptedFileTypes(acceptedFileTypes.stream().toArray(String[]::new));

		upload.addFileRejectedListener(_ -> {
			final String dialogTitle = i18n.getTranslation(MSG_UPLOAD_TITLE);
			final String dialogMsg = i18n.getTranslation(MSG_UPLOAD_ILLEGAL_MIMETYPE);

			new InfoMessageDialog(dialogTitle, dialogMsg, i18n.getLocale()).open();
		});

		if (maxFileSize != null)
			upload.setMaxFileSize(maxFileSize);

		add(upload);
	}

	/**
	 * Constructor
	 * @param locale
	 */
	public FileUploadDialog(Locale locale) {
		this(new InternalI18NService(locale).getTranslation(FILE_UPLOAD_DIALOG_TITLE), null, locale);
	}

	/**
	 * @return the maximum size of a file in bytes
	 */
	public int getMaxFileSize() {
		return maxFileSize;
	}

	/**
	 * @param maxFileSize
	 */
	public void setMaxFileSize(int maxFileSize) {
		this.maxFileSize = maxFileSize;
	}

	/**
	 * @param fileType
	 */
	public void addAcceptedFileType(String fileType) {
		acceptedFileTypes.add(fileType);
	}

	/**
	 * @param uploadFinishedListener
	 */
	public void setUploadFinishedListener(UploadFinishedListener uploadFinishedListener) {
		this.uploadFinishedListener = uploadFinishedListener;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.dialog.AbstractTitleDialog#
	 * addButtons(com.vaadin.flow.component.dialog.Dialog.DialogFooter)
	 */
	@Override
	protected void addButtons(DialogFooter dialogFooter) {
		final var cmdCancel = new Button(i18n.getTranslation(CMD_CANCEL));
		cmdCancel.setId("cmdCancel");

		cmdCancel.addClickListener(_ -> {
			if (upload.isUploading())
				upload.interruptUpload();

			close();
		});

		dialogFooter.add(cmdCancel);
	}

	/**
	 * Method that is invoked as soon as the upload has been finished successfully
	 * @param file
	 * @param fileName
	 */
	protected void onUploadSucceeded(File file, String fileName) {
		logger.debug("Upload operation finished! Generated file {}!", fileName);

		// Notify the receiver that the uploaded file is ready!
		if (uploadFinishedListener != null)
			try {
				uploadFinishedListener.onUploadFinished(file, fileName);
			}
			catch (final Exception e) {
				logger.error("Could not finish the upload!", e);

				final String dialogTitle = i18n.getTranslation(MSG_UPLOAD_TITLE);
				final String dialogMsg = i18n.getTranslation(MSG_UPLOAD_ERROR);

				new ErrorMessageDialog(dialogTitle, dialogMsg, e, i18n.getLocale()).open();
			}

		// Close the dialog automatically as soon as the upload has been finished!
		close();
	}

}
