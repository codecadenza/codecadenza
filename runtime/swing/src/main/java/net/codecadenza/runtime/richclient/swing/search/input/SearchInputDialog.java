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
package net.codecadenza.runtime.richclient.swing.search.input;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SEARCH_INPUT_DIALOG_CANCEL;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SEARCH_INPUT_DIALOG_CLEAR;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SEARCH_INPUT_DIALOG_COUNT;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SEARCH_INPUT_DIALOG_MSG_COUNT_ERROR;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SEARCH_INPUT_DIALOG_MSG_COUNT_RES;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SEARCH_INPUT_DIALOG_MSG_ERROR;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SEARCH_INPUT_DIALOG_SEARCH;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SEARCH_INPUT_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.SEARCH_INPUT_DIALOG_TITLE_MSG;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.lang.invoke.MethodHandles;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;
import net.codecadenza.runtime.richclient.swing.search.Countable;
import net.codecadenza.runtime.richclient.swing.search.input.components.AdvancedSettingsTab;
import net.codecadenza.runtime.richclient.swing.search.input.components.FilterSettingsTab;
import net.codecadenza.runtime.search.dto.SearchDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
public class SearchInputDialog extends JTitleAreaDialog {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = -2556947529124329082L;

	private final SearchDTO searchObj;
	private final transient Countable countable;
	private final FilterSettingsTab filterTab;
	private final AdvancedSettingsTab advancedTab;
	private final JTabbedPane tabs;
	private boolean isErrorState;

	/**
	 * Constructor
	 * @param searchObj the object, where all settings are stored
	 * @param countable a callback to count the number of expected search results
	 * @param filterTab {@link FilterSettingsTab}
	 * @param advancedTab {@link AdvancedSettingsTab}
	 */
	public SearchInputDialog(SearchDTO searchObj, Countable countable, FilterSettingsTab filterTab,
			AdvancedSettingsTab advancedTab) {
		this.searchObj = searchObj;
		this.countable = countable;
		this.filterTab = filterTab;
		this.advancedTab = advancedTab;

		tabs = new JTabbedPane();

		setSize(new Dimension(570, 600));
		setLocationRelativeTo(null);
	}

	/**
	 * Constructor
	 * @param searchObj
	 * @param countable
	 */
	public SearchInputDialog(SearchDTO searchObj, Countable countable) {
		this(searchObj, countable, new FilterSettingsTab(searchObj), new AdvancedSettingsTab(searchObj));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#createContents(javax.swing.JPanel)
	 */
	@Override
	public void createContents(JPanel content) {
		content.setLayout(new BorderLayout());
		content.add(tabs);

		setTitle(getTranslation(SEARCH_INPUT_DIALOG_TITLE));
		setTitleMessage(getTranslation(SEARCH_INPUT_DIALOG_TITLE_MSG));
		setTitleImage(ImageLoader.getImage(ImageLoader.SEARCH_TITLE));

		filterTab.setSearchInputDialog(this);
		filterTab.addTabTo(tabs);

		advancedTab.addTabTo(tabs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#createButtons(javax.swing.JPanel)
	 */
	@Override
	protected void createButtons(JPanel buttonPane) {
		final JButton search = addButton(buttonPane, getTranslation(SEARCH_INPUT_DIALOG_SEARCH));
		getRootPane().setDefaultButton(search);

		search.addActionListener(_ -> onSearchClicked());

		addButton(buttonPane, getTranslation(SEARCH_INPUT_DIALOG_COUNT)).addActionListener(_ -> onCountClicked());
		addButton(buttonPane, getTranslation(SEARCH_INPUT_DIALOG_CLEAR)).addActionListener(_ -> onClearClicked());
		addButton(buttonPane, getTranslation(SEARCH_INPUT_DIALOG_CANCEL), getTranslation(SEARCH_INPUT_DIALOG_CANCEL))
				.addActionListener(_ -> onCancelClicked());
	}

	/**
	 * @param buttonPane
	 * @param text
	 * @param actionCmd
	 * @return the created button
	 */
	private JButton addButton(JPanel buttonPane, String text, String actionCmd) {
		final JButton btn = addButton(buttonPane, text);
		btn.setActionCommand(actionCmd);

		return btn;
	}

	/**
	 * @param buttonPane
	 * @param text
	 * @return the created button
	 */
	private JButton addButton(JPanel buttonPane, String text) {
		final var btn = new JButton(text);
		buttonPane.add(btn);

		return btn;
	}

	/**
	 * Callback method that is executed if the 'Search' button is clicked
	 */
	public void onSearchClicked() {
		if (validateAndSet()) {
			setReturnCode(RETURN_CODE_OK);
			setVisible(false);
		}
	}

	/**
	 * @return true if the validation was successful
	 */
	private boolean validateAndSet() {
		clearError();

		filterTab.validateAndSet();
		advancedTab.validateAndSet();

		return !isErrorState();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#onOKClicked()
	 */
	@Override
	public final void onOKClicked() {
		onSearchClicked();
	}

	/**
	 * Display the number of records that match the given filter criteria
	 */
	public void onCountClicked() {
		if (validateAndSet()) {
			try {
				setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

				final long results = countable.countData(getSearchInput());

				setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));

				JOptionPane.showMessageDialog(this, getTranslation(SEARCH_INPUT_DIALOG_MSG_COUNT_RES, results));
			}
			catch (final Exception ex) {
				logger.error("Error while performing count operation!", ex);

				setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
				JOptionPane.showMessageDialog(this, getTranslation(SEARCH_INPUT_DIALOG_MSG_COUNT_ERROR) + ex.getMessage(),
						getTranslation(SEARCH_INPUT_DIALOG_MSG_ERROR), JOptionPane.ERROR_MESSAGE);
			}
		}
	}

	/**
	 * Reset settings
	 */
	public void onClearClicked() {
		setInformationMessage("");

		filterTab.clear();
		advancedTab.clear();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#onCancelClicked()
	 */
	@Override
	public void onCancelClicked() {
		setReturnCode(RETURN_CODE_CANCEL);
		setVisible(false);
	}

	/**
	 * @return the search object
	 */
	public SearchDTO getSearchInput() {
		return searchObj;
	}

	/**
	 * @return true if an error has occurred
	 */
	public boolean isErrorState() {
		return isErrorState;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#setErrorMessage(java.lang.String)
	 */
	@Override
	public void setErrorMessage(String message) {
		isErrorState = message != null;
		super.setErrorMessage(message);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#setInformationMessage(java.lang.String)
	 */
	@Override
	public void setInformationMessage(String message) {
		isErrorState = false;
		super.setInformationMessage(message);
	}

	/**
	 * Clear a possible error message
	 */
	public void clearError() {
		setInformationMessage(null);
	}

}
