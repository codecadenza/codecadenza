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

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.EXPORT_ACTION_MSG_EXPORT_ERROR;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.EXPORT_ACTION_MSG_TITLE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;

import java.io.File;
import java.lang.invoke.MethodHandles;
import net.codecadenza.runtime.richclient.eclipse.rap.services.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.eclipse.search.__AbstractGridResultPanel;
import net.codecadenza.runtime.richclient.eclipse.search.__AbstractResultView;
import net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView;
import net.codecadenza.runtime.richclient.eclipse.util.DownloadServiceHandler;
import net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.search.dto.SearchDTO;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.rap.rwt.RWT;
import org.eclipse.rap.rwt.client.service.UrlLauncher;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Global action to export table content to Microsoft Excel files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ExportXLSXAction extends AbstractExportXLSXAction {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = 7501121931542115222L;

	/**
	 * Constructor
	 * @param parentShell
	 */
	protected ExportXLSXAction(Shell parentShell) {
		super(parentShell);
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param table
	 * @param searchObj
	 */
	public ExportXLSXAction(Shell parentShell, Table table, SearchDTO searchObj) {
		super(parentShell, table, searchObj);
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param grid
	 */
	public ExportXLSXAction(Shell parentShell, __AbstractDataGridComposite<?> grid) {
		super(parentShell, grid);
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param panel
	 */
	public ExportXLSXAction(Shell parentShell, __AbstractGridResultPanel<?> panel) {
		super(parentShell, panel);
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param view
	 */
	public ExportXLSXAction(Shell parentShell, __AbstractResultView<?> view) {
		super(parentShell, view);
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param searchView
	 */
	public ExportXLSXAction(Shell parentShell, __AbstractSearchResultView<?> searchView) {
		super(parentShell, searchView);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.action.AbstractExportXLSXAction#getFormatPreferences()
	 */
	@Override
	public FormatDTO getFormatPreferences() {
		return FormatPreferencesManager.getFormatDTO();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.action.Action#run()
	 */
	@Override
	public void run() {
		try {
			final File targetFile = File.createTempFile("tmp", DEFAULT_FILE_EXT);

			writeTableContentToXLSFile(targetFile, false);

			final String downloadURL = DownloadServiceHandler.createURL(targetFile.getAbsolutePath());

			final UrlLauncher launcher = RWT.getClient().getService(UrlLauncher.class);
			launcher.openURL(downloadURL);
		}
		catch (final Exception e) {
			logger.error("Error while exporting table data!", e);

			parentShell.setCursor(parentShell.getDisplay().getSystemCursor(SWT.CURSOR_ARROW));
			MessageDialog.openError(parentShell, getTranslation(EXPORT_ACTION_MSG_TITLE),
					getTranslation(EXPORT_ACTION_MSG_EXPORT_ERROR) + e.getMessage());
		}
	}

}
