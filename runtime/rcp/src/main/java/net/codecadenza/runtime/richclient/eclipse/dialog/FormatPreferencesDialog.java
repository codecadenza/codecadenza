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
package net.codecadenza.runtime.richclient.eclipse.dialog;

import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Dialog for changing date and decimal formats
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormatPreferencesDialog extends AbstractFormatPreferencesDialog {
	/**
	 * Constructor
	 * @param parentShell
	 */
	public FormatPreferencesDialog(Shell parentShell) {
		super(parentShell);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.dialog.AbstractFormatPreferencesDialog#getFormatPreferences()
	 */
	@Override
	public FormatDTO getFormatPreferences() {
		return FormatPreferencesManager.getFormatDTO();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.dialog.AbstractFormatPreferencesDialog#
	 * saveFormatPreferences(net.codecadenza.runtime.richclient.format.FormatDTO)
	 */
	@Override
	public void saveFormatPreferences(FormatDTO format) {
		FormatPreferencesManager.saveFormat(format);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#getInitialSize()
	 */
	@Override
	protected Point getInitialSize() {
		return DialogUtility.adaptSizeToSystemDPI(350, 250);
	}

}
