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

import java.util.List;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.event.GUITestActionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Visual editor for maintaining GUI test cases
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GUITestFormEditorDialog extends CodeCadenzaTitleAreaDialog implements GUITestActionListener {
	private final Form form;
	private GUITestFormPreviewBuilder previewBuilder;
	private final GUITestAction testAction;
	private final boolean maintainTestData;
	private final boolean enableDatabaseLookup;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param testAction
	 * @param form
	 * @param maintainTestData
	 * @param enableDatabaseLookup
	 */
	public GUITestFormEditorDialog(Shell parentShell, GUITestAction testAction, Form form, boolean maintainTestData,
			boolean enableDatabaseLookup) {
		super(parentShell);

		this.testAction = testAction;
		this.form = form;
		this.maintainTestData = maintainTestData;
		this.enableDatabaseLookup = enableDatabaseLookup;
	}

	/**
	 * @return the preview builder
	 */
	public GUITestFormPreviewBuilder getPreviewBuilder() {
		return previewBuilder;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite panDialogArea = (Composite) super.createDialogArea(parent);

		previewBuilder = new GUITestFormPreviewBuilder(panDialogArea, testAction, maintainTestData, enableDatabaseLookup);
		previewBuilder.setTestActionListener(this);

		generatePreview();

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(form.getTitle());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.gui.editor.event.GUITestActionListener#onGUITestActionsAdded(java.util.List)
	 */
	@Override
	public void onGUITestActionsAdded(List<GUITestAction> testActions) {
		if (testActions.isEmpty()) {
			setMessage("No test action added!");
			return;
		}

		if (testActions.size() == 1) {
			setMessage("New test action '" + testActions.get(0).getComment() + "' added");
			return;
		}

		setMessage("Added " + testActions.size() + " new test actions");
	}

	/**
	 * Generate the preview
	 */
	private void generatePreview() {
		setMessage("Preview initialized successfully!");

		if (!maintainTestData || form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW
				|| form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
			setTitle("Add new test actions");
		else
			setTitle("Maintain test data and add new test actions");

		try {
			previewBuilder.generateFormPreview(form);
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
			setErrorMessage("The preview could not be created! Message: " + e.getMessage());
		}
	}

}
