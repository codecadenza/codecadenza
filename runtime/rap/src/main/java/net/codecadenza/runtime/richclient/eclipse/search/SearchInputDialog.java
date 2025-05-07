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

import net.codecadenza.runtime.richclient.eclipse.dialog.DialogUtility;
import net.codecadenza.runtime.richclient.eclipse.rap.services.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.eclipse.search.util.SingleLOVProposalProvider;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.search.dto.SearchDTO;
import org.eclipse.jface.fieldassist.ContentProposalAdapter;
import org.eclipse.jface.fieldassist.TextContentAdapter;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for entering filter criteria, sort orders, fetch size and fields to be displayed in a search result view
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchInputDialog extends __SearchInputDialog {
	private static final long serialVersionUID = -7415068684218235227L;

	/**
	 * Create the search input dialog
	 * @param parentShell
	 * @param searchObj
	 * @param countable
	 */
	public SearchInputDialog(Shell parentShell, SearchDTO searchObj, Countable countable) {
		super(parentShell, searchObj, countable);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__SearchInputDialog#getFormatPreferences()
	 */
	@Override
	public FormatDTO getFormatPreferences() {
		return FormatPreferencesManager.getFormatDTO();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__SearchInputDialog#addProposalAdapter(org.eclipse.swt.widgets.Text,
	 * java.lang.String)
	 */
	@Override
	protected void addProposalAdapter(Text textField, String lovCommand) {
		final var apt = new ContentProposalAdapter(textField, new TextContentAdapter(), new SingleLOVProposalProvider(lovCommand),
				null, null);
		apt.setProposalAcceptanceStyle(ContentProposalAdapter.PROPOSAL_REPLACE);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__SearchInputDialog#getInitialSize()
	 */
	@Override
	protected Point getInitialSize() {
		final Point size = super.getInitialSize();

		return DialogUtility.adaptSizeToSystemDPI(size.x, size.y);
	}

}
