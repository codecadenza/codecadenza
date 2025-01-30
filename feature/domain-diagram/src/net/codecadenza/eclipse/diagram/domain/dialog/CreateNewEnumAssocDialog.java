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
package net.codecadenza.eclipse.diagram.domain.dialog;

import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainAttributeValidator;
import net.codecadenza.eclipse.model.domain.DomainFactory;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.EnumAssociation;
import net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.dbsync.util.DBObjectValidationException;
import net.codecadenza.eclipse.tools.dbsync.util.DBSynchService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for creating new enumeration associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateNewEnumAssocDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Create new enumeration association";
	private static final int MAX_FIELD_LENGTH = 255;

	private Text txtName;
	private Text txtColumnName;
	private final EnumAssociation enumAssoc;
	private Combo cboTag;
	private Text txtInternalComment;
	private Text txtUserComment;

	/**
	 * Create the dialog
	 * @param enumAssoc
	 */
	public CreateNewEnumAssocDialog(EnumAssociation enumAssoc) {
		super(Display.getCurrent().getActiveShell());

		this.enumAssoc = enumAssoc;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		final var gdName = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdName.widthHint = 300;

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setText("name");
		txtName.setLayoutData(gdName);
		txtName.selectAll();
		txtName.setFocus();

		txtName.addKeyListener(new KeyListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.KeyAdapter#keyReleased(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyReleased(org.eclipse.swt.events.KeyEvent e) {
				txtColumnName.setText(EclipseIDEService.buildDefaultColumnName(txtName.getText()));
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyPressed(org.eclipse.swt.events.KeyEvent e) {
				// No implementation required!
			}
		});

		final var lblColName = new Label(panDialogArea, SWT.NONE);
		lblColName.setText("Column name:");

		txtColumnName = new Text(panDialogArea, SWT.BORDER);
		txtColumnName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblTag = new Label(panDialogArea, SWT.NONE);
		lblTag.setText("Tag:");

		cboTag = new Combo(panDialogArea, SWT.READ_ONLY);
		cboTag.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		// Add all valid tags
		for (final String tagName : enumAssoc.getSource().getValidAttributeTags())
			if (tagName.equals(AttributeTagEnumeration.NONE.getName()) || tagName.equals(enumAssoc.getTarget().getTag().getName()))
				cboTag.add(tagName);

		cboTag.select(0);

		final var lblUserComment = new Label(panDialogArea, SWT.NONE);
		lblUserComment.setText("User comment:");

		final var gdUserComment = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1);
		gdUserComment.heightHint = 50;

		txtUserComment = new Text(panDialogArea, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtUserComment.setLayoutData(gdUserComment);

		final var lblInternalComment = new Label(panDialogArea, SWT.NONE);
		lblInternalComment.setText("Internal comment:");

		final var gdInternalComment = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1);
		gdInternalComment.heightHint = 50;

		txtInternalComment = new Text(panDialogArea, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtInternalComment.setLayoutData(gdInternalComment);

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			final String name = txtName.getText();
			final DomainObject domainObject = enumAssoc.getSource();
			final String colName = txtColumnName.getText();

			DBTable table = domainObject.getDatabaseTable();

			if (table == null)
				table = domainObject.getRootParentDomainObject(false).getDatabaseTable();

			// Validate the name
			final IStatus status = EclipseIDEService.validateFieldName(name);

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
				return;
			}

			// Check if the attribute name already exist!
			for (final DomainAttribute att : domainObject.getAllAttributes())
				if (att.getName().equals(name)) {
					MessageDialog.openInformation(getShell(), DLG_TITLE, "An attribute with the same name already exists!");
					return;
				}

			for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations())
				if (assoc.getName().equals(name)) {
					MessageDialog.openInformation(getShell(), DLG_TITLE, "An association with the same name already exists!");
					return;
				}

			try {
				// Validate the column name
				new DBSynchService(table.getDatabase()).validateColumnName(table, colName);
			}
			catch (final DBObjectValidationException e) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, e.getMessage());
				return;
			}

			// Create a new domain attribute
			final DomainAttribute attribute = DomainFactory.eINSTANCE.createDomainAttribute();
			domainObject.getAttributes().add(attribute);
			attribute.setDomainObject(domainObject);
			attribute.setName(name);
			attribute.setJavaType(enumAssoc.getTarget());
			attribute.setLabel(EclipseIDEService.buildDefaultLabel(name));
			attribute.setLabelPlural(EclipseIDEService.buildDefaultPluralLabel(name));
			attribute.setTag(AttributeTagEnumeration.valueOf(cboTag.getItem(cboTag.getSelectionIndex())));
			attribute.setInternalComment(txtInternalComment.getText());
			attribute.setUserComment(txtUserComment.getText());
			attribute.setFetchTypeEager(true);

			enumAssoc.setDomainAttribute(attribute);

			// Create the database column
			final DBColumn col = DbFactory.eINSTANCE.createDBColumn();
			col.setName(colName);
			col.setDatabaseTable(table);

			final Project project = attribute.getDomainObject().getNamespace().getProject();
			final JavaType javaType = project.getJavaTypeByName(JavaType.STRING);

			// Get all supported database column types and take the first item in the list!
			final DBColumnType colType = javaType.getDBColumnTypes(project).stream().findFirst().orElse(null);

			if (colType != null) {
				col.setColumnType(colType);
				col.setLength(MAX_FIELD_LENGTH);
			}

			if (domainObject.getParent() != null && !domainObject.equals(domainObject.getRootParentDomainObject(false))
					&& domainObject.getInheritanceType() == InheritanceTypeEnumeration.SINGLE_TABLE)
				col.setNullable(true);

			attribute.setColumn(col);

			// Create a validator
			final DomainAttributeValidator validator = DomainFactory.eINSTANCE.createDomainAttributeValidator();
			validator.setMinLength(-1);
			validator.setMaxLength(MAX_FIELD_LENGTH);
			validator.setMinValue("");
			validator.setMaxValue("");
			validator.setRegularExpression("");

			attribute.setDomainAttributeValidator(validator);
		}

		super.buttonPressed(buttonId);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(DLG_TITLE);
	}

}
