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
package net.codecadenza.runtime.richclient.eclipse.search;

import net.codecadenza.runtime.richclient.eclipse.action.AbstractExportXLSXAction;
import net.codecadenza.runtime.richclient.eclipse.action.ExportXLSXAction;
import net.codecadenza.runtime.richclient.eclipse.rap.services.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Abstract base class to display the query results in a view
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data grid
 */
public abstract class AbstractResultView<T> extends __AbstractResultView<T> {
	protected ExportXLSXAction exportAction;

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractResultView#__createActions()
	 */
	@Override
	protected void __createActions() {
		super.__createActions();

		exportAction = new ExportXLSXAction(parentShell, this);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractResultView#getExportAction()
	 */
	@Override
	protected AbstractExportXLSXAction getExportAction() {
		return exportAction;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractResultView#__initializeToolBar()
	 */
	@Override
	protected void __initializeToolBar() {
		super.__initializeToolBar();

		toolBarManager.add(exportAction);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractResultView#init(org.eclipse.swt.widgets.Composite,
	 * org.eclipse.swt.widgets.Shell)
	 */
	@Override
	public void init(Composite parent, Shell parentShell) {
		super.init(parent, parentShell);

		__createActions();
		__initializeToolBar();

		refreshView();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractResultView#getFormatPreferences()
	 */
	@Override
	public FormatDTO getFormatPreferences() {
		return FormatPreferencesManager.getFormatDTO();
	}

	/**
	 * Dispose internal resources
	 */
	public void dispose() {

	}

}
