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
package net.codecadenza.runtime.richclient.swing.search.actions;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SEARCH_INPUT_ACTION_MSG_ERROR_SETTINGS;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SEARCH_INPUT_ACTION_NAME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SEARCH_INPUT_ACTION_SHORT_DESC;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;
import net.codecadenza.runtime.richclient.swing.search.input.SearchInputDialog;
import net.codecadenza.runtime.richclient.swing.search.panel.AbstractSearchResultDataPanel;
import net.codecadenza.runtime.richclient.swing.search.util.SearchDTOs;

/**
 * <p>
 * Action to open the search input dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchInputAction extends AbstractAction {
	private static final long serialVersionUID = -7246497630279475565L;

	private final AbstractSearchResultDataPanel<?> panel;

	/**
	 * Constructor
	 * @param panel
	 */
	public SearchInputAction(AbstractSearchResultDataPanel<?> panel) {
		super(getTranslation(SEARCH_INPUT_ACTION_NAME), ImageLoader.getImage(ImageLoader.SEARCH));

		this.panel = panel;

		putValue(SHORT_DESCRIPTION, getTranslation(SEARCH_INPUT_ACTION_SHORT_DESC));
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent event) {
		panel.refreshFormatSettings();

		// Open the dialog for entering search data
		final var dlg = new SearchInputDialog(SearchDTOs.deepClone(panel.getSearchObj()), panel.getCountable());

		dlg.setModal(true);
		dlg.setVisible(true);

		if (dlg.getReturnCode() == JTitleAreaDialog.RETURN_CODE_CANCEL)
			return;

		if (panel.getSearchObj().getSearchFields().size() != dlg.getSearchInput().getSearchFields().size())
			throw new RuntimeException(getTranslation(SEARCH_INPUT_ACTION_MSG_ERROR_SETTINGS));

		panel.rebuildTable(dlg.getSearchInput());
		panel.setPageIndex(1);

		// Perform the data fetch operation
		panel.getDataFetchAction().actionPerformed(null);
	}

}
