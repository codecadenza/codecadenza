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
package net.codecadenza.runtime.richclient.eclipse.widget;

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CHECKBOX_DATA_GRID_COMPOSITE_CMD_DESELECT_ALL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CHECKBOX_DATA_GRID_COMPOSITE_CMD_SELECT_ALL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

/**
 * <p>
 * Checkbox data grid
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the checkbox data grid
 */
public class __CheckboxDataGridComposite<T> extends __AbstractDataGridComposite<T> {
	protected CheckboxTableViewer checkboxViewer;

	/**
	 * Constructor
	 * @param parent
	 */
	public __CheckboxDataGridComposite(Composite parent) {
		super(parent);

		this.parentShell = parent.getShell();
		this.setLayout(new FillLayout());

		checkboxViewer = CheckboxTableViewer.newCheckList(this, SWT.HIDE_SELECTION | SWT.BORDER);

		tableViewer = checkboxViewer;
		tableViewer.addDoubleClickListener(event -> onDoubleClick(getSelection()));
		tableViewer.setLabelProvider(new TableLabelProvider());
		tableViewer.setContentProvider(new ContentProvider());

		table = tableViewer.getTable();
		table.setLinesVisible(true);
		table.setHeaderVisible(true);

		popUpMenu = new Menu(table);
		table.setMenu(popUpMenu);
	}

	/**
	 * Set all elements checked
	 */
	@SuppressWarnings("unchecked")
	public final void checkAllElements() {
		final var input = (Collection<T>) tableViewer.getInput();
		checkboxViewer.setCheckedElements(input.toArray());
	}

	/**
	 * Set all elements unchecked
	 */
	public final void uncheckAllElements() {
		checkboxViewer.setCheckedElements(new Object[0]);
	}

	/**
	 * @return all checked elements
	 */
	@SuppressWarnings("unchecked")
	public final List<T> getCheckedElements() {
		final var list = new ArrayList<T>();
		final Object[] checkedElements = checkboxViewer.getCheckedElements();

		for (final Object elem : checkedElements)
			list.add((T) elem);

		return list;
	}

	/**
	 * Set elements to be checked
	 * @param input
	 */
	public final void setCheckedElements(Collection<T> input) {
		checkboxViewer.setCheckedElements(input.toArray());
	}

	/**
	 * Add default pop-up menu items
	 */
	protected void initPopUpMenu() {
		final var mniSelAll = new MenuItem(popUpMenu, SWT.NONE);
		mniSelAll.setText(getTranslation(CHECKBOX_DATA_GRID_COMPOSITE_CMD_SELECT_ALL));

		mniSelAll.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(final SelectionEvent e) {
				checkAllElements();

				// Fire an event that the selection has changed!
				final var event = new Event();
				event.type = SWT.Selection;
				event.widget = checkboxViewer.getTable();

				checkboxViewer.getTable().notifyListeners(SWT.Selection, event);
			}
		});

		final var mniDeselAll = new MenuItem(popUpMenu, SWT.NONE);
		mniDeselAll.setText(getTranslation(CHECKBOX_DATA_GRID_COMPOSITE_CMD_DESELECT_ALL));

		mniDeselAll.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(final SelectionEvent e) {
				uncheckAllElements();

				// Fire an event that the selection has changed!
				final var event = new Event();
				event.type = SWT.Selection;
				event.widget = checkboxViewer.getTable();

				checkboxViewer.getTable().notifyListeners(SWT.Selection, event);
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.widgets.Control#setToolTipText(java.lang.String)
	 */
	@Override
	public void setToolTipText(String toolTipText) {
		super.setToolTipText(toolTipText);

		checkboxViewer.getTable().setToolTipText(toolTipText);
	}

}
