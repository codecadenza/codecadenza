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

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DUAL_DATA_SELECTION_LIST_COMP_CMD_DESELECT_ALL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DUAL_DATA_SELECTION_LIST_COMP_CMD_SELECT_ALL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DUAL_DATA_SELECTION_LIST_COMP_LBL_ALL_ITEMS;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DUAL_DATA_SELECTION_LIST_COMP_LBL_SEARCH_ITEMS;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DUAL_DATA_SELECTION_LIST_COMP_LBL_SEL_ITEMS;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslationForFieldLabel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dual data list composite
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the selection list
 */
public class DualDataSelectionListComposite<T> extends Composite {
	private final ListViewer listSelectedItems;
	private final ListViewer listAllItems;
	private final Cursor handCursor;
	protected Shell parentShell;

	/**
	 * Constructor
	 * @param parent
	 * @param style
	 */
	public DualDataSelectionListComposite(Composite parent, int style) {
		this(parent, style, true);
	}

	/**
	 * Constructor
	 * @param parent
	 * @param style
	 * @param useNaturalOrdering if set to true the selected items will be sorted according to their natural ordering
	 */
	public DualDataSelectionListComposite(Composite parent, int style, boolean useNaturalOrdering) {
		super(parent, style);

		this.parentShell = parent.getShell();
		this.setLayout(new GridLayout(3, false));
		this.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		handCursor = new Cursor(parentShell.getDisplay(), SWT.CURSOR_HAND);

		final var gdSearch = new GridData(SWT.FILL, SWT.FILL, false, false, 3, 1);

		final var groupSearch = new Group(this, SWT.NONE);
		groupSearch.setLayoutData(gdSearch);
		groupSearch.setLayout(new GridLayout(3, false));

		final var lblSearchItems = new Label(groupSearch, SWT.NONE);
		lblSearchItems.setText(getTranslation(DUAL_DATA_SELECTION_LIST_COMP_LBL_SEARCH_ITEMS));

		final var txtSearchInput = new Text(groupSearch, SWT.BORDER);
		txtSearchInput.setLayoutData(new GridData(195, SWT.DEFAULT));

		txtSearchInput.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent e) {
				refreshListWithAllItems(txtSearchInput.getText());
			}
		});

		final var lblPerformSearch = new Label(groupSearch, SWT.NONE);
		lblPerformSearch.setImage(ImageCache.getImage("search_list.png"));
		lblPerformSearch.setCursor(handCursor);

		lblPerformSearch.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDown(MouseEvent e) {
				refreshListWithAllItems(txtSearchInput.getText());
			}
		});

		final var lblAllItems = new Label(this, SWT.NONE);
		lblAllItems.setText(getTranslationForFieldLabel(DUAL_DATA_SELECTION_LIST_COMP_LBL_ALL_ITEMS));

		new Label(this, SWT.NONE);

		final var lblSelItems = new Label(this, SWT.NONE);
		lblSelItems.setText(getTranslationForFieldLabel(DUAL_DATA_SELECTION_LIST_COMP_LBL_SEL_ITEMS));

		listAllItems = new ListViewer(this, SWT.V_SCROLL | SWT.MULTI | SWT.H_SCROLL | SWT.BORDER);
		listAllItems.addDoubleClickListener(event -> doSelect());
		listAllItems.setLabelProvider(new ListLabelProvider());
		listAllItems.setContentProvider(new ListContentProvider());
		listAllItems.setComparator(new ItemComparator());

		final List listAll = listAllItems.getList();
		listAll.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var popUpAll = new Menu(listAll);
		listAll.setMenu(popUpAll);

		final var mniAllSelAll = new MenuItem(popUpAll, SWT.NONE);
		mniAllSelAll.setText(getTranslation(DUAL_DATA_SELECTION_LIST_COMP_CMD_SELECT_ALL));

		mniAllSelAll.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			@SuppressWarnings("unchecked")
			public void widgetSelected(final SelectionEvent e) {
				if (listAllItems.getInput() == null)
					return;

				// Add all available items to the items that are already selected
				final Collection<T> selectedItems = getSelectedItems();
				final var newItems = new ArrayList<>((Collection<T>) listAllItems.getInput());

				// Avoid adding a list item twice!
				selectedItems.addAll(newItems.stream().filter(i -> selectedItems.stream().noneMatch(d -> d.equals(i))).toList());

				listSelectedItems.setInput(selectedItems);
			}
		});

		final var panButtons = new Composite(this, SWT.NONE);
		panButtons.setLayout(new GridLayout());

		final var cmdSelect = new Button(panButtons, SWT.NONE);
		cmdSelect.setImage(ImageCache.getImage(ImageCache.IMG_SELECT));

		cmdSelect.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				doSelect();
			}
		});

		final var cmdDeselect = new Button(panButtons, SWT.NONE);
		cmdDeselect.setImage(ImageCache.getImage(ImageCache.IMG_DESELECT));

		cmdDeselect.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				doDeselect();
			}
		});

		listSelectedItems = new ListViewer(this, SWT.V_SCROLL | SWT.MULTI | SWT.H_SCROLL | SWT.BORDER);
		listSelectedItems.addDoubleClickListener(event -> doDeselect());
		listSelectedItems.setLabelProvider(new ListLabelProvider());
		listSelectedItems.setContentProvider(new ListContentProvider());

		if (useNaturalOrdering)
			listSelectedItems.setComparator(new ItemComparator());

		final List listSel = listSelectedItems.getList();
		listSel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var popUpSel = new Menu(listSel);
		listSel.setMenu(popUpSel);

		final var mniDeselectAll = new MenuItem(popUpSel, SWT.NONE);
		mniDeselectAll.setText(getTranslation(DUAL_DATA_SELECTION_LIST_COMP_CMD_DESELECT_ALL));

		mniDeselectAll.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(final SelectionEvent e) {
				listSelectedItems.setInput(null);
			}
		});
	}

	/**
	 * Content provider
	 */
	private class ListContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Object[] getElements(Object inputElement) {
			return ((Collection<T>) inputElement).toArray();
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
		 */
		@Override
		public void dispose() {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// No implementation required!
		}
	}

	/**
	 * Label provider
	 */
	private class ListLabelProvider extends LabelProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public String getText(Object element) {
			final T item = (T) element;
			return getItemText(item);
		}
	}

	/**
	 * Compare items
	 * @param item1
	 * @param item2
	 * @return the result of the comparison
	 */
	public int compareItems(T item1, T item2) {
		return getItemText(item1).compareTo(getItemText(item2));
	}

	/**
	 * Comparator that compares two items by using the method compareItems()
	 */
	private class ItemComparator extends ViewerComparator {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ViewerComparator#compare(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public int compare(Viewer viewer, Object e1, Object e2) {
			final T item1 = (T) e1;
			final T item2 = (T) e2;

			return compareItems(item1, item2);
		}
	}

	/**
	 * Get the item text. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @return the item text
	 */
	@SuppressWarnings("unused")
	public String getItemText(T element) {
		return "";
	}

	/**
	 * @param filter text
	 * @return the items found
	 */
	@SuppressWarnings("unused")
	public Collection<T> searchItems(String filter) {
		return Collections.emptyList();
	}

	/**
	 * Refresh the list that contains all available items
	 * @param filter
	 */
	protected void refreshListWithAllItems(String filter) {
		final Collection<T> data;

		if (filter.isEmpty())
			data = Collections.emptyList();
		else
			data = searchItems(filter);

		listAllItems.setInput(data);
	}

	/**
	 * Set the selected items
	 * @param selItems
	 */
	public void setSelectedItems(Collection<T> selItems) {
		final var input = new ArrayList<T>();

		selItems.forEach(input::add);

		listSelectedItems.setInput(input);
	}

	/**
	 * Set the available items
	 * @param items
	 */
	public void setAvailableItems(Collection<T> items) {
		listAllItems.setInput(new ArrayList<>(items));
	}

	/**
	 * @return all selected items
	 */
	@SuppressWarnings("unchecked")
	public Collection<T> getSelectedItems() {
		if (listSelectedItems.getInput() == null)
			return new ArrayList<>();

		return (Collection<T>) listSelectedItems.getInput();
	}

	/**
	 * Select items
	 */
	@SuppressWarnings("unchecked")
	private void doSelect() {
		final var selection = (StructuredSelection) listAllItems.getSelection();

		if (selection == null)
			return;

		final Collection<T> selectedItems = getSelectedItems();
		final Collection<T> newItems = new ArrayList<>(selection.toList());

		// Avoid adding a list item twice!
		selectedItems.addAll(newItems.stream().filter(e -> selectedItems.stream().noneMatch(d -> d.equals(e))).toList());

		listSelectedItems.setInput(selectedItems);
	}

	/**
	 * Deselect items
	 */
	private void doDeselect() {
		final var selection = (StructuredSelection) listSelectedItems.getSelection();

		if (selection == null)
			return;

		final Collection<T> selectedItems = getSelectedItems();
		selectedItems.removeAll(selection.toList());

		listSelectedItems.setInput(selectedItems);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.widgets.Control#setToolTipText(java.lang.String)
	 */
	@Override
	public void setToolTipText(String toolTipText) {
		listSelectedItems.getList().setToolTipText(toolTipText);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.widgets.Widget#dispose()
	 */
	@Override
	public void dispose() {
		super.dispose();
		handCursor.dispose();
	}

}
