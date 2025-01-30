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
package net.codecadenza.eclipse.ui.dialog.testing.gui.editor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.action.RowCountDialog;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.action.SearchInputDialog;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.event.GUITestActionListener;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.GUITestFieldPreviewBuilderFactory;
import net.codecadenza.eclipse.ui.dialog.testing.gui.util.GUITestActionUtil;
import net.codecadenza.eclipse.ui.preview.AbstractFormPreviewBuilder;
import net.codecadenza.runtime.richclient.eclipse.widget.DataGridComposite;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.ToolItem;

/**
 * <p>
 * Preview builder for the GUI test form editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GUITestFormPreviewBuilder extends AbstractFormPreviewBuilder {
	private final GUITestAction testAction;
	private final List<GUITestAction> testActions = new ArrayList<>();
	private final boolean maintainTestData;
	private GUITestActionListener testActionListener;
	private final boolean enableDatabaseLookup;

	/**
	 * Constructor
	 * @param panParent
	 * @param testAction
	 * @param maintainTestData
	 * @param enableDatabaseLookup
	 */
	public GUITestFormPreviewBuilder(Composite panParent, GUITestAction testAction, boolean maintainTestData,
			boolean enableDatabaseLookup) {
		super(panParent);

		this.testAction = testAction;
		this.maintainTestData = maintainTestData;
		this.enableDatabaseLookup = enableDatabaseLookup;
	}

	/**
	 * @param testActionListener
	 */
	public void setTestActionListener(GUITestActionListener testActionListener) {
		this.testActionListener = testActionListener;
	}

	/**
	 * @return a list with all test actions defined by this preview builder
	 */
	public List<GUITestAction> getTestActions() {
		return testActions;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.AbstractFormPreviewBuilder#addFormFields(org.eclipse.swt.widgets.Composite,
	 * net.codecadenza.eclipse.model.client.FormPanel)
	 */
	@Override
	protected void addFormFields(Composite panParent, FormPanel formPanel) {
		// Rearrange all fields in order to avoid display problems because of inappropriate layout settings
		formPanel.rearrangeFields();

		// Add the fields to the panel
		for (final FormField formField : formPanel.getFields()) {
			if (formField.isHidden())
				continue;

			// Check if a test data object exists for every field. Create a test data object if it cannot be found!
			GUITestData testData = testAction.getTestData().stream()
					.filter(existingTestData -> formField.equals(existingTestData.getFormField())).findFirst().orElse(null);

			if (testData == null) {
				testData = TestingFactory.eINSTANCE.createGUITestData();
				testData.setFormField(formField);
				testData.setTestAction(testAction);
				testData.setType(GUITestDataType.FORM_FIELD);
			}

			GUITestFieldPreviewBuilderFactory.getFieldBuilder(panParent, testData, maintainTestData, enableDatabaseLookup)
					.generateFieldPreview();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.AbstractFormPreviewBuilder#onToolBarItemRefreshClick(net.codecadenza.eclipse.model.
	 * client.FormPanel)
	 */
	@Override
	protected void onToolBarItemRefreshClick(FormPanel gridPanel) {
		final GUITestAction pageRefreshAction = TestingFactory.eINSTANCE.createGUITestAction();
		pageRefreshAction.setForm(testAction.getForm());
		pageRefreshAction.setFormPanel(gridPanel);
		pageRefreshAction.setType(GUITestActionType.EXECUTE_REFRESH_ACTION);

		// Create a comment
		GUITestActionUtil.initTestActionComment(pageRefreshAction);

		testActions.add(pageRefreshAction);

		// Inform all listeners about a new test action
		if (testActionListener != null)
			testActionListener.onGUITestActionsAdded(Collections.singletonList(pageRefreshAction));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.AbstractFormPreviewBuilder#onToolBarItemSearchClick()
	 */
	@Override
	protected void onToolBarItemSearchClick() {
		final GUITestAction searchInputAction = TestingFactory.eINSTANCE.createGUITestAction();
		searchInputAction.setForm(testAction.getForm());

		final var dlg = new SearchInputDialog(shell, searchInputAction, true, enableDatabaseLookup);
		final int returnCode = dlg.open();

		if (returnCode != Window.OK)
			return;

		searchInputAction.setType(dlg.getSelectedActionType());

		// Create a comment
		GUITestActionUtil.initTestActionComment(searchInputAction);

		testActions.add(searchInputAction);

		// Inform all listeners about a new test action
		if (testActionListener != null)
			testActionListener.onGUITestActionsAdded(Collections.singletonList(searchInputAction));
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * net.codecadenza.eclipse.ui.preview.AbstractFormPreviewBuilder#generateGridPanelPreview(org.eclipse.swt.widgets.Composite,
	 * net.codecadenza.eclipse.model.client.FormPanel)
	 */
	@Override
	protected DataGridComposite<?> generateGridPanelPreview(Composite panParentComp, FormPanel gridPanel) {
		final Composite panMain = panParentComp == null ? panParent : panParentComp;

		// Add a toolbar
		addToolBar(panMain, gridPanel, gridPanel.getActions(), false);

		// Create a data grid
		final DataGridComposite<?> dataGrid = initDataGrid(panMain, gridPanel, true);

		// Add the footer
		generateGridFooter(panMain);

		return dataGrid;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.AbstractFormPreviewBuilder#addGridPanelMenuItems(org.eclipse.swt.widgets.Menu,
	 * net.codecadenza.runtime.richclient.eclipse.widget.DataGridComposite, net.codecadenza.eclipse.model.client.FormPanel)
	 */
	@Override
	protected void addGridPanelMenuItems(Menu gridPanelMenu, DataGridComposite<?> dataGrid, FormPanel gridPanel) {
		final var mniRowCount = new MenuItem(gridPanelMenu, SWT.NONE);
		mniRowCount.setText("Validate row count");

		mniRowCount.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final var dlg = new RowCountDialog(shell, form, gridPanel);
				final int returnCode = dlg.open();

				if (returnCode != Window.OK)
					return;

				testActions.add(dlg.getRowCountValidationAction());

				// Inform all listeners about a new test action
				if (testActionListener != null)
					testActionListener.onGUITestActionsAdded(Collections.singletonList(dlg.getRowCountValidationAction()));
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.AbstractFormPreviewBuilder#addToolBarItemSelectionListener(org.eclipse.swt.widgets.
	 * ToolItem, net.codecadenza.eclipse.model.client.FormAction, boolean)
	 */
	@Override
	protected void addToolBarItemSelectionListener(ToolItem toolItem, FormAction formAction, boolean editAction) {
		toolItem.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final List<GUITestAction> newActions = GUITestActionUtil.initGUITestActionsOfFormAction(testAction.getForm(), formAction,
						enableDatabaseLookup);

				testActions.addAll(newActions);

				// Inform all listeners about new test actions
				if (testActionListener != null)
					testActionListener.onGUITestActionsAdded(newActions);
			}
		});
	}

}
