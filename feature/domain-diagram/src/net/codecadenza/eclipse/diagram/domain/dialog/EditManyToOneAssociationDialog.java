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
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.dbsync.util.DBObjectValidationException;
import net.codecadenza.eclipse.tools.dbsync.util.DBSynchService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for creating and maintaining many-to-one associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditManyToOneAssociationDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_EDIT = "Edit many-to-one association";
	private static final String DLG_TITLE_NEW = "Create new many-to-one association";

	private Text txtDBColumnName;
	private Text txtAssociationName;
	private Button chkCascadePersist;
	private Button chkCascadeMerge;
	private Button chkCascadeRefresh;
	private Button chkCascadeRemove;
	private Button chkFetchTypeEager;
	private Button chkOptional;
	private Button chkInsertable;
	private Button chkUpdatable;
	private final DomainObject target;
	private final ManyToOneAssociation association;
	private final boolean doEdit;
	private Combo cboTag;
	private Text txtInternalComment;
	private Text txtUserComment;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param doEdit
	 * @param target
	 * @param association
	 */
	public EditManyToOneAssociationDialog(Shell parentShell, boolean doEdit, DomainObject target,
			ManyToOneAssociation association) {
		super(parentShell);

		this.doEdit = doEdit;
		this.target = target;
		this.association = association;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 4);

		final var lblAssociationName = new Label(panDialogArea, SWT.NONE);
		lblAssociationName.setText("Name:");

		txtAssociationName = new Text(panDialogArea, SWT.BORDER);
		txtAssociationName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		txtAssociationName.addFocusListener(new FocusAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
			 */
			@Override
			public void focusLost(FocusEvent e) {
				txtDBColumnName.setText(EclipseIDEService.buildDefaultColumnName(txtAssociationName.getText()));
			}
		});

		final var lblDBColumnName = new Label(panDialogArea, SWT.NONE);
		lblDBColumnName.setText("Join column name:");

		txtDBColumnName = new Text(panDialogArea, SWT.BORDER);
		txtDBColumnName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDBColumnName.setEnabled(!doEdit);

		final var lblOptional = new Label(panDialogArea, SWT.NONE);
		lblOptional.setText("Optional:");

		chkOptional = new Button(panDialogArea, SWT.CHECK);

		final var lblFetchTypeEager = new Label(panDialogArea, SWT.NONE);
		lblFetchTypeEager.setText("Fetch type eager:");

		chkFetchTypeEager = new Button(panDialogArea, SWT.CHECK);

		final var lblCascadePersist = new Label(panDialogArea, SWT.NONE);
		lblCascadePersist.setText("Cascade persist:");

		chkCascadePersist = new Button(panDialogArea, SWT.CHECK);

		final var lblCascadeMerge = new Label(panDialogArea, SWT.NONE);
		lblCascadeMerge.setText("Cascade merge:");

		chkCascadeMerge = new Button(panDialogArea, SWT.CHECK);

		final var lblCascadeRefresh = new Label(panDialogArea, SWT.NONE);
		lblCascadeRefresh.setText("Cascade refresh:");

		chkCascadeRefresh = new Button(panDialogArea, SWT.CHECK);

		final var lblCascadeRemove = new Label(panDialogArea, SWT.NONE);
		lblCascadeRemove.setText("Cascade remove:");

		chkCascadeRemove = new Button(panDialogArea, SWT.CHECK);

		final var lblInsertable = new Label(panDialogArea, SWT.NONE);
		lblInsertable.setText("Insertable:");

		chkInsertable = new Button(panDialogArea, SWT.CHECK);

		final var lblUpdatable = new Label(panDialogArea, SWT.NONE);
		lblUpdatable.setText("Updatable:");

		chkUpdatable = new Button(panDialogArea, SWT.CHECK);

		if (!doEdit) {
			final var lblTag = new Label(panDialogArea, SWT.NONE);
			lblTag.setText("Tag:");

			cboTag = new Combo(panDialogArea, SWT.READ_ONLY);
			cboTag.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			association.getDomainObject().getValidAssociationTags(target).forEach(cboTag::add);

			if (cboTag.getItemCount() > 1)
				cboTag.select(1);
			else
				cboTag.select(0);

			new Label(panDialogArea, SWT.NONE);
			new Label(panDialogArea, SWT.NONE);
		}

		final var lblUserComment = new Label(panDialogArea, SWT.NONE);
		lblUserComment.setText("User comment:");

		final var gdUserComment = new GridData(SWT.FILL, SWT.FILL, true, true, 3, 1);
		gdUserComment.heightHint = 50;

		txtUserComment = new Text(panDialogArea, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtUserComment.setLayoutData(gdUserComment);

		final var lblInternalComment = new Label(panDialogArea, SWT.NONE);
		lblInternalComment.setText("Internal comment:");

		final var gdInternalComment = new GridData(SWT.FILL, SWT.FILL, true, true, 3, 1);
		gdInternalComment.heightHint = 50;

		txtInternalComment = new Text(panDialogArea, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtInternalComment.setLayoutData(gdInternalComment);

		if (!doEdit) {
			chkUpdatable.setSelection(true);
			chkInsertable.setSelection(true);

			final String assocName = target.getName().substring(0, 1).toLowerCase() + target.getName().substring(1);
			txtAssociationName.setText(assocName);
			txtDBColumnName.setText(EclipseIDEService.buildDefaultColumnName(txtAssociationName.getText()));

			// By default, a self-referencing association should be optional and loaded lazily!
			if (association.getTarget().equals(association.getDomainObject()))
				chkOptional.setSelection(true);
			else
				chkFetchTypeEager.setSelection(true);
		}
		else {
			chkUpdatable.setSelection(association.isUpdatable());
			chkInsertable.setSelection(association.isInsertable());
			chkCascadeMerge.setSelection(association.isCascadeMerge());
			chkCascadePersist.setSelection(association.isCascadePersist());
			chkCascadeRefresh.setSelection(association.isCascadeRefresh());
			chkCascadeRemove.setSelection(association.isCascadeRemove());
			txtDBColumnName.setText(association.getColumn().getName());
			txtAssociationName.setText(association.getName());
			chkFetchTypeEager.setSelection(association.isFetchTypeEager());
			chkOptional.setSelection(association.isOptional());
			txtInternalComment.setText(association.getInternalComment() == null ? "" : association.getInternalComment());
			txtUserComment.setText(association.getUserComment() == null ? "" : association.getUserComment());

			if (!association.isOwner()) {
				chkCascadeMerge.setEnabled(false);
				chkCascadePersist.setEnabled(false);
				chkCascadeRefresh.setEnabled(false);
				chkCascadeRemove.setEnabled(false);
				txtDBColumnName.setEnabled(false);
				txtAssociationName.setEnabled(false);
				chkFetchTypeEager.setEnabled(false);
			}
		}

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
	 */
	@Override
	protected Point getInitialSize() {
		return new Point(Math.max(600, super.getInitialSize().x), super.getInitialSize().y);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID && !writeInputsToObject())
			return;

		super.buttonPressed(buttonId);
	}

	/**
	 * Apply the input
	 * @return true if the validation was successful
	 */
	private boolean writeInputsToObject() {
		AssociationTagEnumeration assocTag = null;

		if (doEdit && !association.isOwner()) {
			association.setInsertable(chkInsertable.getSelection());
			association.setUpdatable(chkUpdatable.getSelection());
			association.setOptional(chkOptional.getSelection());
			association.getColumn().setNullable(chkOptional.getSelection());
			association.setInternalComment(txtInternalComment.getText());
			association.setUserComment(txtUserComment.getText());

			return true;
		}

		String messageTitle = DLG_TITLE_NEW;

		if (doEdit)
			messageTitle = DLG_TITLE_EDIT;
		else
			assocTag = AssociationTagEnumeration.valueOf(cboTag.getItem(cboTag.getSelectionIndex()));

		final IStatus status = EclipseIDEService.validateFieldName(txtAssociationName.getText());

		if (status.getSeverity() > IStatus.INFO) {
			MessageDialog.openInformation(getShell(), messageTitle, status.getMessage());
			txtAssociationName.setFocus();
			return false;
		}

		if (!doEdit)
			try {
				final Database db = target.getNamespace().getProject().getDatabase();
				DBTable table = association.getDomainObject().getDatabaseTable();

				if (table == null)
					table = association.getDomainObject().getRootParentDomainObject(false).getDatabaseTable();

				new DBSynchService(db).validateColumnName(table, txtDBColumnName.getText());
			}
			catch (final DBObjectValidationException ex) {
				MessageDialog.openInformation(getShell(), messageTitle, ex.getMessage());

				txtDBColumnName.setFocus();
				return false;
			}

		if (!chkInsertable.getSelection() && !chkOptional.getSelection()) {
			MessageDialog.openInformation(getShell(), messageTitle, "A non-optional assocation must be insertable!");
			return false;
		}

		if (assocTag == AssociationTagEnumeration.CLIENT_REFERENCE && chkOptional.getSelection()) {
			final var messageText = "An association with a 'CLIENT_REFERENCE' tag must not be optional!";

			MessageDialog.openInformation(getShell(), messageTitle, messageText);
			return false;
		}

		association.setCascadeMerge(chkCascadeMerge.getSelection());
		association.setCascadePersist(chkCascadePersist.getSelection());
		association.setCascadeRefresh(chkCascadeRefresh.getSelection());
		association.setCascadeRemove(chkCascadeRemove.getSelection());
		association.setFetchTypeEager(chkFetchTypeEager.getSelection());
		association.setName(txtAssociationName.getText());
		association.setInsertable(chkInsertable.getSelection());
		association.setUpdatable(chkUpdatable.getSelection());
		association.setOptional(chkOptional.getSelection());
		association.setInternalComment(txtInternalComment.getText());
		association.setUserComment(txtUserComment.getText());

		if (!doEdit) {
			final DBColumn col = DbFactory.eINSTANCE.createDBColumn();
			association.setColumn(col);
			association.setOwner(true);
			association.setTag(assocTag);
			association.getColumn().setName(txtDBColumnName.getText());
		}

		association.getColumn().setNullable(chkOptional.getSelection());

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		if (doEdit)
			newShell.setText(DLG_TITLE_EDIT);
		else
			newShell.setText(DLG_TITLE_NEW);
	}

	/**
	 * @return the association
	 */
	public ManyToOneAssociation getAssociation() {
		return association;
	}

}
