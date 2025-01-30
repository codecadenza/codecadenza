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

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SAVE_QUERY_ACTION_MSG_ENTER_NAME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SAVE_QUERY_ACTION_MSG_MISSING_NAME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SAVE_QUERY_ACTION_MSG_OVERWRITE_QUERY;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SAVE_QUERY_ACTION_MSG_QUERY_DUPLICATE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SAVE_QUERY_ACTION_MSG_QUERY_SAVED;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SAVE_QUERY_ACTION_MSG_QUERY_SAVE_ERROR;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SAVE_QUERY_ACTION_MSG_SAVE_TITLE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SAVE_QUERY_ACTION_NAME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SAVE_QUERY_ACTION_SHORT_DESC;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SAVE_QUERY_ACTION_TITLE_OVERWRITE_QUERY;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.event.ActionEvent;
import java.lang.invoke.MethodHandles;
import javax.swing.AbstractAction;
import javax.swing.JOptionPane;
import net.codecadenza.runtime.richclient.search.util.DuplicateSearchNameException;
import net.codecadenza.runtime.richclient.search.util.SearchManager;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;
import net.codecadenza.runtime.richclient.swing.search.panel.AbstractSearchResultDataPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Action to save a query
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SaveQueryAction extends AbstractAction {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = -7445530142772242465L;

	private final AbstractSearchResultDataPanel<?> panel;

	/**
	 * Constructor
	 * @param panel
	 */
	public SaveQueryAction(AbstractSearchResultDataPanel<?> panel) {
		super(getTranslation(SAVE_QUERY_ACTION_NAME), ImageLoader.getImage(ImageLoader.SAVE_AS));

		this.panel = panel;

		putValue(SHORT_DESCRIPTION, getTranslation(SAVE_QUERY_ACTION_SHORT_DESC));
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		if (panel.isUserDefQuery()) {
			final int resp = JOptionPane.showConfirmDialog(panel, getTranslation(SAVE_QUERY_ACTION_MSG_OVERWRITE_QUERY),
					getTranslation(SAVE_QUERY_ACTION_TITLE_OVERWRITE_QUERY), JOptionPane.YES_NO_OPTION);

			if (resp == JOptionPane.YES_OPTION)
				SearchManager.overwriteSavedSearchObject(panel.getSearchObjectId(), panel.getSearchObj());
		}
		else {
			// Open dialog for entering a name
			final String resp = JOptionPane.showInputDialog(panel, getTranslation(SAVE_QUERY_ACTION_MSG_ENTER_NAME),
					getTranslation(SAVE_QUERY_ACTION_MSG_SAVE_TITLE), JOptionPane.DEFAULT_OPTION);

			if (resp == null)
				return;

			if (resp.isEmpty()) {
				panel.setStatusErrorMessage(getTranslation(SAVE_QUERY_ACTION_MSG_MISSING_NAME));
				return;
			}

			try {
				SearchManager.saveSearch(panel.getViewID(), panel.getSearchObj(), resp);
				panel.setStatusInfoMessage(getTranslation(SAVE_QUERY_ACTION_MSG_QUERY_SAVED));
			}
			catch (final DuplicateSearchNameException ex) {
				panel.setStatusErrorMessage(getTranslation(SAVE_QUERY_ACTION_MSG_QUERY_DUPLICATE, resp));
			}
			catch (final Exception ex) {
				logger.error("Error while saving query!", ex);

				panel.setStatusErrorMessage(getTranslation(SAVE_QUERY_ACTION_MSG_QUERY_SAVE_ERROR) + ex.getMessage());
			}
		}
	}

}
