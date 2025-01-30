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
package net.codecadenza.runtime.richclient.swing.search.input.components;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.FILTER_SETTINGS_TAB_NAME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.Container;
import java.awt.GridBagLayout;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import net.codecadenza.runtime.richclient.swing.search.input.SearchInputDialog;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldTypeEnum;

/**
 * <p>
 * Represents the tab where the user can configure filters on the data
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FilterSettingsTab implements Serializable {
	private static final long serialVersionUID = 4572594914542578935L;

	private final Container pane;
	private final List<Criterion> criteria;
	private final SearchDTO searchObj;
	private SearchInputDialog dlg;

	/**
	 * Constructor
	 * @param searchObj
	 */
	public FilterSettingsTab(SearchDTO searchObj) {
		this.pane = new JPanel();
		this.criteria = new ArrayList<>();
		this.searchObj = searchObj;
	}

	/**
	 * Add a tab folder for the filter fields
	 * @param tabs
	 */
	public void addTabTo(JTabbedPane tabs) {
		if (dlg == null)
			throw new IllegalStateException();

		tabs.addTab(getTranslation(FILTER_SETTINGS_TAB_NAME), new JScrollPane(pane));

		// Sort the search fields
		searchObj.getSearchFields().sort((o1, o2) -> o1.getOriginalColumnIndex() - o2.getOriginalColumnIndex());

		searchObj.getSearchFields().stream().filter(field -> field.getType() != SearchFieldTypeEnum.NOT_SEARCHABLE).forEach(field -> {
			final Criterion c = Criterion.createCriterion(field, pane, criteria.size());
			c.setSearchInputDialog(dlg);

			criteria.add(c);
		});

		final var layout = new GridBagLayout();
		layout.columnWidths = new int[] { 1, 80, 240, 50, 50, 50 };
		layout.columnWeights = new double[] { 0, 0, 0, 0, 0, 1 }; // expand only last column
		layout.rowWeights = expandLastRow(criteria.size());

		pane.setLayout(layout);

		int n = 0;

		for (final Criterion c : criteria)
			c.addToGrid(++n == criteria.size());
	}

	/**
	 * Build an array where every element is zero except the last one, which is 1, used in the {@link GridBagLayout} to give the
	 * last row all the remaining size
	 * @param size
	 * @return an array containing row sizes
	 */
	private double[] expandLastRow(int size) {
		final var ws = new double[size];
		Arrays.fill(ws, 0);
		ws[size - 1] = 1;

		return ws;
	}

	/**
	 * Reset filter criteria
	 */
	public void clear() {
		criteria.forEach(Criterion::clear);
	}

	/**
	 * Set the dialog where this tab folder is contained in
	 * @param dlg
	 */
	public void setSearchInputDialog(SearchInputDialog dlg) {
		this.dlg = dlg;
	}

	/**
	 * Validate the input and inform the user about errors
	 */
	public void validateAndSet() {
		for (final Criterion c : criteria) {
			if (dlg.isErrorState())
				break;

			c.validateAndSet();
		}
	}

}
