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

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FETCH_NEXT_PAGE_ACTION_NAME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FETCH_NEXT_PAGE_ACTION_SHORT_DESC;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;
import net.codecadenza.runtime.richclient.swing.search.panel.AbstractSearchResultDataPanel;

/**
 * <p>
 * Action to fetch the next search result page
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FetchNextPageAction extends AbstractAction {
	private static final long serialVersionUID = 4699781316254255118L;

	private final AbstractSearchResultDataPanel<?> panel;

	/**
	 * Constructor
	 * @param panel
	 */
	public FetchNextPageAction(AbstractSearchResultDataPanel<?> panel) {
		super(getTranslation(FETCH_NEXT_PAGE_ACTION_NAME), ImageLoader.getImage(ImageLoader.VIEW_NEXT));

		this.panel = panel;

		putValue(SHORT_DESCRIPTION, getTranslation(FETCH_NEXT_PAGE_ACTION_SHORT_DESC));
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent event) {
		panel.setPageIndex(panel.getPageIndex() + 1);

		// Perform the data fetch operation
		panel.performDataFetch();
	}

}
