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
package net.codecadenza.eclipse.tools.reverse.dialog;

import static net.codecadenza.eclipse.shared.Constants.REMOVE_DEFAULT_MENU_ITEMS;

import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.reverse.model.RevEngEnum;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import net.codecadenza.runtime.richclient.eclipse.widget.DataGridComposite;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for creating and maintaining Java enumerations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditJavaEnumDialog extends CodeCadenzaDialog {
	private static final Color READ_ONLY_COLOR = Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND);
	private static final String DLG_TITLE_EDIT = "Edit enum";
	private static final String DLG_TITLE_NEW = "Create new enum";
	private static final String NEW_LITERAL_NAME = "NEW_LITERAL";

	private final JavaEnum javaEnum;
	private final Project project;
	private Text txtName;
	private String title = DLG_TITLE_NEW;
	private boolean editMode;
	private DataGridComposite<EnumLiteral> gridLiterals;
	private boolean readonly;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 */
	public EditJavaEnumDialog(Shell parentShell, Project project) {
		super(parentShell);

		this.project = project;
		this.javaEnum = JavaFactory.eINSTANCE.createJavaEnum();
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param revEngEnum
	 * @param project
	 */
	public EditJavaEnumDialog(Shell parentShell, RevEngEnum revEngEnum, Project project) {
		super(parentShell);

		this.javaEnum = revEngEnum.getJavaEnum();
		this.project = project;
		this.editMode = true;
		this.readonly = !revEngEnum.isCreatedByReverseEngineering();
		this.title = DLG_TITLE_EDIT;
	}

	/**
	 * @return the Java enum
	 */
	public JavaEnum getJavaEnum() {
		return javaEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblAssociationName = new Label(panDialogArea, SWT.NONE);
		lblAssociationName.setText("Name:");

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblLiterals = new Label(panDialogArea, SWT.NONE);
		lblLiterals.setText("Literals:");

		new Label(panDialogArea, SWT.NONE);

		gridLiterals = new DataGridComposite<>(panDialogArea, SWT.BORDER | SWT.FULL_SELECTION, REMOVE_DEFAULT_MENU_ITEMS) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(EnumLiteral element, int columnIndex) {
				return element.getName();
			}
		};

		final var gdLiterals = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1);
		gdLiterals.heightHint = 300;

		gridLiterals.addColumn("Name", ColumnSortType.STRING, 400);
		gridLiterals.setLayoutData(gdLiterals);

		if (!readonly) {
			final var mniAdd = new MenuItem(gridLiterals.getPopUpMenu(), SWT.NONE);
			mniAdd.setText("Add");

			mniAdd.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					final var dlg = new InputDialog(getShell(), "Add literal", "Enter the name:", NEW_LITERAL_NAME, newText -> {
						final IStatus status = EclipseIDEService.validateFieldName(newText);

						if (status.getSeverity() > IStatus.INFO)
							return status.getMessage();

						return javaEnum.getEnumerationValues().stream().filter(literal -> literal.getName().equals(newText)).findFirst()
								.map(literal -> "A literial with the same name already exists!").orElse(null);
					});

					if (dlg.open() != Window.OK)
						return;

					final String literalName = dlg.getValue();

					final EnumLiteral literal = JavaFactory.eINSTANCE.createEnumLiteral();
					literal.setName(literalName);
					literal.setTag(EnumLiteralTagEnumeration.NONE);
					literal.setJavaEnum(javaEnum);

					javaEnum.getEnumerationValues().add(literal);

					// Refresh the table
					gridLiterals.setData(javaEnum.getEnumerationValues());
				}
			});

			final var mniDelete = new MenuItem(gridLiterals.getPopUpMenu(), SWT.NONE);
			mniDelete.setText("Delete");

			mniDelete.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					final EnumLiteral literal = gridLiterals.getSelection();

					if (literal == null)
						return;

					javaEnum.getEnumerationValues().remove(literal);

					// Refresh the table
					gridLiterals.setData(javaEnum.getEnumerationValues());
				}
			});
		}

		if (editMode) {
			txtName.setText(javaEnum.getName());
			gridLiterals.setData(javaEnum.getEnumerationValues());
		}

		if (readonly) {
			txtName.setEditable(false);
			txtName.setBackground(READ_ONLY_COLOR);
		}

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		if (!readonly)
			createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);

		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID && !validateAndSaveInput())
			return;

		super.buttonPressed(buttonId);
	}

	/**
	 * Validate the input and save the data if the validation was successful
	 * @return true if the validation was successful
	 */
	private boolean validateAndSaveInput() {
		final IStatus status = EclipseIDEService.validateFieldName(txtName.getText());

		if (status.getSeverity() > IStatus.INFO) {
			MessageDialog.openInformation(getShell(), title, status.getMessage());
			txtName.setFocus();
			return false;
		}

		// Check if the name is unique within the selected namespace
		for (final Namespace ns : project.getDomainNamespace().getChildNamespaces())
			for (final JavaType type : ns.getJavaTypes()) {
				if (type.equals(javaEnum))
					continue;

				if (type.getName().equals(javaEnum.getName())) {
					MessageDialog.openInformation(getShell(), title, "A class with the same name already exists!");
					return false;
				}
			}

		javaEnum.setName(txtName.getText());
		javaEnum.setMappable(true);
		javaEnum.setComment("");

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(title);
	}

}
