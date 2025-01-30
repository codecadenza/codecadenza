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
package net.codecadenza.eclipse.ui.preview;

import static net.codecadenza.eclipse.shared.Constants.IMG_ARROW_LEFT;
import static net.codecadenza.eclipse.shared.Constants.IMG_ARROW_RIGHT;
import static net.codecadenza.eclipse.shared.Constants.IMG_DOWNLOAD;
import static net.codecadenza.eclipse.shared.Constants.IMG_EXPORT_CSV;
import static net.codecadenza.eclipse.shared.Constants.IMG_REFRESH;
import static net.codecadenza.eclipse.shared.Constants.IMG_SEARCH;
import static net.codecadenza.eclipse.shared.Constants.IMG_UPLOAD;
import static net.codecadenza.eclipse.shared.Constants.IMG_WINDOW_CREATE;
import static net.codecadenza.eclipse.shared.Constants.IMG_WINDOW_EDIT;
import static net.codecadenza.eclipse.shared.Constants.IMG_WINDOW_VIEW;
import static net.codecadenza.eclipse.shared.Constants.REMOVE_DEFAULT_MENU_ITEMS;

import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormPanelComparator;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldComparator;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.runtime.richclient.eclipse.widget.DataGridComposite;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Abstract base class for all form preview builders
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractFormPreviewBuilder {
	protected final Composite panParent;
	protected FormTypeEnumeration formType;
	protected Form form;
	protected Project project;
	protected final Shell shell = Display.getCurrent().getActiveShell();
	protected final ISharedImages workbenchImages = PlatformUI.getWorkbench().getSharedImages();
	protected int selectionIndexTabFolder1;
	protected int selectionIndexTabFolder2;
	protected int emptyRowIndex;

	/**
	 * Constructor
	 * @param panParent
	 */
	protected AbstractFormPreviewBuilder(Composite panParent) {
		this.panParent = panParent;
	}

	/**
	 * Every derived class must define how fields should be added to the given form panel
	 * @param panParent
	 * @param formPanel
	 */
	protected abstract void addFormFields(Composite panParent, FormPanel formPanel);

	/**
	 * Add the menu items for a grid panel. The method provides no implementation as it is up to the derived class to define the
	 * respective functionality!
	 * @param gridPanelMenu
	 * @param dataGrid
	 * @param gridPanel
	 */
	@SuppressWarnings("unused")
	protected void addGridPanelMenuItems(Menu gridPanelMenu, DataGridComposite<?> dataGrid, FormPanel gridPanel) {

	}

	/**
	 * Add the menu items for a form panel. The method provides no implementation as it is up to the derived class to define the
	 * respective functionality!
	 * @param mnuPanel
	 * @param formPanel
	 * @param hasGrid
	 */
	@SuppressWarnings("unused")
	protected void addFormPanelMenuItems(Menu mnuPanel, FormPanel formPanel, boolean hasGrid) {

	}

	/**
	 * Add listeners for toolbar item click events. The method provides no implementation as it is up to the derived class to define
	 * the respective functionality!
	 * @param toolItem
	 * @param action
	 * @param editAction
	 */
	@SuppressWarnings("unused")
	protected void addToolBarItemSelectionListener(ToolItem toolItem, FormAction action, boolean editAction) {

	}

	/**
	 * Callback listener that is fired as soon as a user clicks on the toolbar item 'Refresh'
	 * @param gridPanel
	 */
	@SuppressWarnings("unused")
	protected void onToolBarItemRefreshClick(FormPanel gridPanel) {

	}

	/**
	 * Callback listener that is fired as soon as a user clicks on the toolbar item 'Search'
	 */
	protected void onToolBarItemSearchClick() {

	}

	/**
	 * Generate the preview for the given form
	 * @param form
	 */
	public void generateFormPreview(Form form) {
		this.form = form;
		this.formType = form.getFormType();
		this.project = form.getDomainObject().getNamespace().getProject();

		if (formType == FormTypeEnumeration.SEARCHABLE_VIEW || formType == FormTypeEnumeration.SIMPLE_VIEW
				|| formType == FormTypeEnumeration.LOV)
			generateViewFormPreview(panParent);
		else
			generateUpdateFormPreview(panParent);
	}

	/**
	 * Generate the preview for the given grid panel
	 * @param gridPanel
	 */
	public void generateGridPanelPreview(FormPanel gridPanel) {
		this.formType = FormTypeEnumeration.GRID;
		this.project = gridPanel.getAssociation().getDomainObject().getNamespace().getProject();

		generateGridPanelPreview(null, gridPanel);
	}

	/**
	 * Generate the preview for the given grid panel
	 * @param panParentComp
	 * @param gridPanel
	 * @return the new data grid
	 */
	protected DataGridComposite<?> generateGridPanelPreview(Composite panParentComp, FormPanel gridPanel) {
		final Composite panMain = panParentComp == null ? panParent : panParentComp;
		final boolean allowEdit = panParentComp == null;

		// Add a toolbar
		addToolBar(panMain, gridPanel, gridPanel.getActions(), allowEdit);

		// Create the data grid
		final DataGridComposite<?> dataGrid = initDataGrid(panMain, gridPanel, allowEdit);

		// Add the footer
		generateGridFooter(panMain);

		return dataGrid;
	}

	/**
	 * Generate the preview for a view form
	 * @param panParent
	 */
	protected void generateViewFormPreview(Composite panParent) {
		final var gdMain = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdMain.widthHint = 800;
		gdMain.heightHint = 500;

		final var panMain = new Composite(panParent, SWT.NONE);
		panMain.setLayout(new GridLayout());
		panMain.setLayoutData(gdMain);

		// Add a toolbar
		addToolBar(panMain, null, form.getActions(), true);

		if (formType == FormTypeEnumeration.LOV) {
			final var panLOV = new Composite(panMain, SWT.NONE);
			panLOV.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
			panLOV.setLayout(new GridLayout(3, false));

			// Add controls to filter items displayed in a list-of-values
			final var lblSearch = new Label(panLOV, SWT.NONE);
			lblSearch.setText("Filter:");
			lblSearch.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));

			final var gdSearchInput = new GridData(SWT.LEFT, SWT.CENTER, false, false);
			gdSearchInput.widthHint = 164;

			final var txtSearchInput = new Text(panLOV, SWT.BORDER | SWT.READ_ONLY);
			txtSearchInput.setLayoutData(gdSearchInput);

			final var cmdSearch = new Button(panLOV, SWT.NONE);
			cmdSearch.setText("Search");
		}

		initDataGrid(panMain, null, true);

		generateGridFooter(panMain);
	}

	/**
	 * Create and initialize the data grid
	 * @param panParent
	 * @param gridPanel
	 * @param allowEdit
	 * @return the new data grid
	 */
	protected DataGridComposite<?> initDataGrid(Composite panParent, FormPanel gridPanel, boolean allowEdit) {
		final FormTable formTable = gridPanel != null ? gridPanel.getFormTable() : form.getFormPanels().get(0).getFormTable();

		final var gdDataGrid = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdDataGrid.widthHint = 700;
		gdDataGrid.heightHint = 400;

		final var dataGrid = new DataGridComposite<>(panParent, SWT.BORDER | SWT.FULL_SELECTION, REMOVE_DEFAULT_MENU_ITEMS);
		dataGrid.setLayoutData(gdDataGrid);

		// Sort the table columns
		ECollections.sort(formTable.getFields(), new TableColumnFieldComparator());

		// Add all visible table columns
		for (final TableColumnField col : formTable.getFields()) {
			if (!col.isVisible())
				continue;

			dataGrid.addColumn(col.getTitle(), col.getWidth());
		}

		if (allowEdit)
			addGridPanelMenuItems(dataGrid.getPopUpMenu(), dataGrid, gridPanel);

		return dataGrid;
	}

	/**
	 * Generate a preview for forms of type 'ADD', 'CREATE', 'UPDATE' and 'READONLY'
	 * @param parent
	 */
	protected void generateUpdateFormPreview(Composite parent) {
		final var panelsOfFirstRow = new BasicEList<FormPanel>();
		final var panelsOfSecondRow = new BasicEList<FormPanel>();

		for (final FormPanel panel : form.getFormPanels())
			if (panel.getRowIndex() == 1)
				panelsOfFirstRow.add(panel);
			else
				panelsOfSecondRow.add(panel);

		final var panPreview = new Composite(parent, SWT.NONE);
		panPreview.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		panPreview.setLayout(new GridLayout());

		Composite panContainer = panPreview;

		// Add a toolbar
		addToolBar(panPreview, null, form.getActions(), true);

		// Sort the panels of the first row
		ECollections.sort(panelsOfFirstRow, new FormPanelComparator());

		// Check if the form needs a tab folder
		if (panelsOfFirstRow.size() > 1) {
			final var tabFolder1 = new TabFolder(panPreview, SWT.NONE);

			tabFolder1.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					selectionIndexTabFolder1 = tabFolder1.getSelectionIndex();
				}
			});

			panContainer = tabFolder1;

			if (!panelsOfSecondRow.isEmpty())
				tabFolder1.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
			else
				tabFolder1.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		}

		// Add the panels of the first row
		for (final FormPanel panel : panelsOfFirstRow)
			addPanelToForm(panContainer, panel, panelsOfSecondRow.isEmpty());

		// Restore the selection of the tab folder in the first row
		if (panContainer instanceof final TabFolder tabFolder)
			tabFolder.setSelection(selectionIndexTabFolder1);

		// Sort the panels of the second row
		ECollections.sort(panelsOfSecondRow, new FormPanelComparator());

		// Add a tab folder for the second row if the first row uses a tab folder
		if ((panelsOfFirstRow.size() > 1 && !panelsOfSecondRow.isEmpty()) || panelsOfSecondRow.size() > 1) {
			final var tabFolder2 = new TabFolder(panPreview, SWT.NONE);
			tabFolder2.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

			tabFolder2.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					selectionIndexTabFolder2 = tabFolder2.getSelectionIndex();
				}
			});

			panContainer = tabFolder2;
		}

		// Add the panels of the second row
		for (final FormPanel formPanel : panelsOfSecondRow)
			addPanelToForm(panContainer, formPanel, true);

		// Restore the selection of the tab folder in the second row
		if (panContainer instanceof final TabFolder tabFolder && panelsOfSecondRow.size() > 1)
			tabFolder.setSelection(selectionIndexTabFolder2);
	}

	/**
	 * Create a preview of the form panel and add it to the parent panel
	 * @param panParent
	 * @param formPanel
	 * @param grabVerticalSpace
	 * @return the new panel
	 */
	protected Composite addPanelToForm(Composite panParent, FormPanel formPanel, boolean grabVerticalSpace) {
		boolean hasGrid = false;
		Composite panContent;

		if (formPanel.getBasePanel() != null)
			hasGrid = true;

		if (panParent instanceof final TabFolder tabFolder) {
			final var tabItem = new TabItem(tabFolder, SWT.NONE);
			tabItem.setText(formPanel.getLabel());

			panContent = new Composite(panParent, SWT.NONE);
			panContent.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

			if (!hasGrid)
				panContent.setLayout(new GridLayout(4, false));
			else {
				panContent.setLayout(new GridLayout());

				// Add a grid panel
				final DataGridComposite<?> dataGrid = generateGridPanelPreview(panContent, formPanel.getBasePanel());

				addFormPanelMenuItems(dataGrid.getPopUpMenu(), formPanel, hasGrid);
			}

			tabItem.setControl(panContent);
		}
		else {
			if (formPanel.isDrawBorder()) {
				panContent = new Group(panParent, SWT.NONE);
				((Group) panContent).setText(formPanel.getLabel());
			}
			else
				panContent = new Composite(panParent, SWT.NONE);

			if (!hasGrid) {
				panContent.setLayout(new GridLayout(4, false));

				if (grabVerticalSpace)
					panContent.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
				else
					panContent.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
			}
			else {
				panContent.setLayout(new GridLayout());
				panContent.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

				// Add a grid panel
				final DataGridComposite<?> dataGrid = generateGridPanelPreview(panContent, formPanel.getBasePanel());

				addFormPanelMenuItems(dataGrid.getPopUpMenu(), formPanel, hasGrid);
			}
		}

		if (!hasGrid) {
			final var mnuPanel = new Menu(panContent);
			panContent.setMenu(mnuPanel);

			addFormPanelMenuItems(mnuPanel, formPanel, hasGrid);

			addFormFields(panContent, formPanel);
		}

		return panContent;
	}

	/**
	 * Add an item to the toolbar
	 * @param toolBar
	 * @param action
	 * @param editAction
	 */
	protected void addToolBarItem(ToolBar toolBar, FormAction action, boolean editAction) {
		// Don't add toolbar items for manipulating data in case of a read-only grid panel!
		if (form != null && form.getFormType() == FormTypeEnumeration.READONLY && (action.getType() == ActionType.CREATE
				|| action.getType() == ActionType.UPDATE || action.getType() == ActionType.DELETE || action.getType() == ActionType.COPY))
			return;

		final var toolItem = new ToolItem(toolBar, SWT.NONE);

		if (action.getDescription() != null)
			toolItem.setToolTipText(action.getDescription());

		if (action.getType() == ActionType.DOWNLOAD || action.getType() == ActionType.DOWNLOAD_EXPORT)
			toolItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_DOWNLOAD));
		else if (action.getType() == ActionType.INDIRECT_UPLOAD || action.getType() == ActionType.UPLOAD_IMPORT
				|| action.getType() == ActionType.DIRECT_UPLOAD)
			toolItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_UPLOAD));
		else if (action.getType() == ActionType.CREATE)
			toolItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_CREATE));
		else if (action.getType() == ActionType.UPDATE)
			toolItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_EDIT));
		else if (action.getType() == ActionType.READ)
			toolItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_VIEW));
		else if (action.getType() == ActionType.DELETE)
			toolItem.setImage(workbenchImages.getImage(ISharedImages.IMG_TOOL_DELETE));
		else if (action.getType() == ActionType.COPY)
			toolItem.setImage(workbenchImages.getImage(ISharedImages.IMG_TOOL_COPY));

		addToolBarItemSelectionListener(toolItem, action, editAction);
	}

	/**
	 * Add a toolbar control to the given parent composite
	 * @param panParent
	 * @param gridPanel
	 * @param actions
	 * @param allowEdit
	 */
	protected void addToolBar(Composite panParent, FormPanel gridPanel, EList<FormAction> actions, boolean allowEdit) {
		final ToolBar toolBar;

		if (formType == FormTypeEnumeration.SEARCHABLE_VIEW || formType == FormTypeEnumeration.SIMPLE_VIEW
				|| formType == FormTypeEnumeration.GRID || !allowEdit) {
			toolBar = new ToolBar(panParent, SWT.RIGHT);

			// Add standard items
			final var itemRefresh = new ToolItem(toolBar, SWT.NONE);
			itemRefresh.setImage(CodeCadenzaResourcePlugin.getImage(IMG_REFRESH));
			itemRefresh.setToolTipText("Refresh");

			itemRefresh.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					onToolBarItemRefreshClick(gridPanel);
				}
			});

			final var itemStop = new ToolItem(toolBar, SWT.NONE);
			itemStop.setImage(workbenchImages.getImage(ISharedImages.IMG_ELCL_STOP));
			itemStop.setToolTipText("Stop");
			itemStop.setEnabled(false);

			final var itemExport = new ToolItem(toolBar, SWT.NONE);
			itemExport.setImage(CodeCadenzaResourcePlugin.getImage(IMG_EXPORT_CSV));
			itemExport.setToolTipText("Export");

			if (formType == FormTypeEnumeration.SEARCHABLE_VIEW) {
				final var itemSearch = new ToolItem(toolBar, SWT.NONE);
				itemSearch.setImage(CodeCadenzaResourcePlugin.getImage(IMG_SEARCH));
				itemSearch.setToolTipText("Search");

				itemSearch.addSelectionListener(new SelectionAdapter() {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
					 */
					@Override
					public void widgetSelected(SelectionEvent e) {
						onToolBarItemSearchClick();
					}
				});

				final var itemPrev = new ToolItem(toolBar, SWT.NONE);
				itemPrev.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ARROW_LEFT));
				itemPrev.setToolTipText("Previous");

				final var itemNext = new ToolItem(toolBar, SWT.NONE);
				itemNext.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ARROW_RIGHT));
				itemNext.setToolTipText("Next");
			}

			// Add toolbar items for form actions
			actions.forEach(action -> addToolBarItem(toolBar, action, allowEdit));
		}
		else {
			// We will add a toolbar to a single-record form only if it contains file-related actions!
			final boolean addToolbar = actions.stream().anyMatch(a -> a.getType() == ActionType.DOWNLOAD
					|| a.getType() == ActionType.INDIRECT_UPLOAD || a.getType() == ActionType.DIRECT_UPLOAD);

			if (!addToolbar)
				return;

			toolBar = new ToolBar(panParent, SWT.RIGHT);

			for (final FormAction a : actions)
				if (a.getType() == ActionType.DOWNLOAD || a.getType() == ActionType.INDIRECT_UPLOAD
						|| a.getType() == ActionType.DIRECT_UPLOAD)
					addToolBarItem(toolBar, a, false);
		}
	}

	/**
	 * Generate the footer for view forms and grid panels
	 * @param panParent
	 */
	protected void generateGridFooter(Composite panParent) {
		final var panFooter = new Composite(panParent, SWT.BORDER);
		panFooter.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		panFooter.setLayout(new GridLayout(2, false));

		final var lblStateImage = new Label(panFooter, SWT.NONE);
		lblStateImage.setImage(workbenchImages.getImage(ISharedImages.IMG_OBJS_INFO_TSK));

		final var lblResult = new Label(panFooter, SWT.NONE);
		lblResult.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));
		lblResult.setText("Status information area");
	}

}
