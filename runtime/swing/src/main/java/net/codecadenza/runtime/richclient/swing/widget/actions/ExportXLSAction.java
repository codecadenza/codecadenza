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
package net.codecadenza.runtime.richclient.swing.widget.actions;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.EXPORT_ACTION_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.EXPORT_ACTION_MSG_EXPORT_ERROR;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.EXPORT_ACTION_MSG_EXPORT_FINISHED;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.EXPORT_ACTION_MSG_TITLE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.EXPORT_ACTION_NAME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.EXPORT_TO_XLS_ACTION_FILTER_NAME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.EXPORT_TO_XLS_ACTION_SHORT_DESC;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.io.File;
import java.lang.invoke.MethodHandles;
import javax.swing.AbstractAction;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;
import net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Action to export the table content to a Microsoft Excel 2007 file
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ExportXLSAction extends AbstractAction {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = -3693967055737711180L;
	private static final String DEFAULT_FILE_EXT = ".xlsx";

	private final AbstractDataTablePanel<?> panel;

	/**
	 * Constructor
	 * @param panel
	 */
	public ExportXLSAction(AbstractDataTablePanel<?> panel) {
		super(getTranslation(EXPORT_ACTION_NAME), ImageLoader.getImage(ImageLoader.EXPORT_EXCEL));

		this.panel = panel;
		putValue(SHORT_DESCRIPTION, getTranslation(EXPORT_TO_XLS_ACTION_SHORT_DESC));
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		File targetFile = null;

		try {
			if (Desktop.isDesktopSupported())
				targetFile = File.createTempFile("tmp", DEFAULT_FILE_EXT);
			else {
				// Open a file selection dialog
				final var fc = new JFileChooser();
				fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
				fc.setDialogTitle(getTranslation(EXPORT_ACTION_DIALOG_TITLE));
				fc.setFileFilter(new FileNameExtensionFilter(getTranslation(EXPORT_TO_XLS_ACTION_FILTER_NAME), DEFAULT_FILE_EXT));

				if (fc.showSaveDialog(panel) != JFileChooser.APPROVE_OPTION)
					return;

				targetFile = fc.getSelectedFile();

				// Create a new file if it doesn't exist or overwrite an existing file!
				if (!targetFile.exists() && !targetFile.createNewFile())
					logger.warn("The file {} already exists!", targetFile.getName());
			}

			panel.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			panel.getTable().writeTableContentToXLSFile(targetFile);
			panel.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));

			if (Desktop.isDesktopSupported())
				Desktop.getDesktop().open(targetFile);
			else
				JOptionPane.showMessageDialog(panel, getTranslation(EXPORT_ACTION_MSG_EXPORT_FINISHED),
						getTranslation(EXPORT_ACTION_MSG_TITLE), JOptionPane.INFORMATION_MESSAGE);
		}
		catch (final Exception ex) {
			logger.error("Error while exporting table data!", ex);

			panel.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			JOptionPane.showMessageDialog(panel, getTranslation(EXPORT_ACTION_MSG_EXPORT_ERROR) + ex.getMessage(),
					getTranslation(EXPORT_ACTION_MSG_TITLE), JOptionPane.ERROR_MESSAGE);
		}
		finally {
			panel.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}

}
