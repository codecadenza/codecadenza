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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainObject;
import net.codecadenza.eclipse.tools.reverse.service.DomainModelValidationService;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining domain objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditDomainObjectDialog extends CodeCadenzaDialog {
	private static final Color READ_ONLY_COLOR = Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND);
	private static final String DLG_TITLE = "Edit domain object";
	private static final String DLG_TITLE_INPUT_VAL = "Input validation";
	private static final int DEFAULT_BLOCK_SIZE = 20;
	private static final int DEFAULT_INIT_VALUE = 1;

	private final DomainObject domainObject;
	private final Set<String> sequences;
	private final List<IDGeneratorTypeEnumeration> idGenTypes = new ArrayList<>();
	private Text txtComment;
	private Text txtLabelPlural;
	private Text txtNamePlural;
	private Text txtBlockSize;
	private Text txtInitialValue;
	private DataComboViewer<IDGeneratorTypeEnumeration> cboIdGeneratorType;
	private Button chkPropertyAccess;
	private Text txtLabel;
	private Text txtDomainObjectName;
	private DataComboViewer<String> cboSequence;
	private final boolean editMode;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param revEngObj
	 * @param sequences
	 * @param project
	 */
	public EditDomainObjectDialog(Shell parentShell, RevEngDomainObject revEngObj, Set<String> sequences, Project project) {
		super(parentShell);

		this.domainObject = revEngObj.getDomainObject();
		this.sequences = sequences;
		this.editMode = revEngObj.isCreatedByReverseEngineering();

		for (final IDGeneratorTypeEnumeration type : IDGeneratorTypeEnumeration.values()) {
			if (type == IDGeneratorTypeEnumeration.SEQUENCE && !project.getDatabase().isSupportsSequence() && sequences.isEmpty())
				continue;

			if (type == IDGeneratorTypeEnumeration.IDENTITY && !project.getDatabase().isSupportsIdentityColumn())
				continue;

			idGenTypes.add(type);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var groupBasicData = new Group(panDialogArea, SWT.NONE);
		groupBasicData.setLayout(new GridLayout(2, false));
		groupBasicData.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		groupBasicData.setText("Basic data");

		final var lblName = new Label(groupBasicData, SWT.LEFT);
		lblName.setText("Domain object name:");

		txtDomainObjectName = new Text(groupBasicData, SWT.SINGLE | SWT.BORDER);
		txtDomainObjectName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDomainObjectName.setFocus();

		txtDomainObjectName.addFocusListener(new FocusAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
			 */
			@Override
			public void focusLost(FocusEvent e) {
				if (txtDomainObjectName.getText().equals(domainObject.getName()))
					return;

				txtNamePlural.setText(EclipseIDEService.buildDefaultPluralForm(txtDomainObjectName.getText()));
				txtLabel.setText(EclipseIDEService.buildDefaultLabel(txtDomainObjectName.getText()));
				txtLabelPlural.setText(EclipseIDEService.buildDefaultPluralLabel(txtDomainObjectName.getText()));
				txtComment.setText(EclipseIDEService.buildDomainObjectComment(txtLabel.getText()));
			}
		});

		final var lblNamePlural = new Label(groupBasicData, SWT.NONE);
		lblNamePlural.setText("Plural name:");

		txtNamePlural = new Text(groupBasicData, SWT.SINGLE | SWT.BORDER);
		txtNamePlural.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblLabel = new Label(groupBasicData, SWT.NONE);
		lblLabel.setText("Label:");

		txtLabel = new Text(groupBasicData, SWT.BORDER);
		txtLabel.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		txtLabel.addFocusListener(new FocusAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
			 */
			@Override
			public void focusLost(FocusEvent e) {
				if (txtLabel.getText().equals(domainObject.getLabel()))
					return;

				txtLabelPlural.setText(EclipseIDEService.buildDefaultPluralForm(txtLabel.getText()));
				txtComment.setText(EclipseIDEService.buildDomainObjectComment(txtLabel.getText()));
			}
		});

		final var lblLabelPlural = new Label(groupBasicData, SWT.NONE);
		lblLabelPlural.setText("Plural label:");

		txtLabelPlural = new Text(groupBasicData, SWT.BORDER);
		txtLabelPlural.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblTableName = new Label(groupBasicData, SWT.LEFT);
		lblTableName.setText("Table name:");

		final var txtTableName = new Text(groupBasicData, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
		txtTableName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtTableName.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));

		final var lblPropertyAccess = new Label(groupBasicData, SWT.NONE);
		lblPropertyAccess.setText("Property access:");

		chkPropertyAccess = new Button(groupBasicData, SWT.CHECK);

		final var lblComment = new Label(groupBasicData, SWT.NONE);
		lblComment.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));
		lblComment.setText("Comment:");

		final var gdComment = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdComment.heightHint = 100;
		gdComment.widthHint = 400;

		txtComment = new Text(groupBasicData, SWT.V_SCROLL | SWT.H_SCROLL | SWT.MULTI | SWT.BORDER);
		txtComment.setLayoutData(gdComment);

		final var groupIDGenerator = new Group(panDialogArea, SWT.NONE);
		groupIDGenerator.setLayout(new GridLayout(2, false));
		groupIDGenerator.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		groupIDGenerator.setText("ID generator");

		final var lblIdGeneratorType = new Label(groupIDGenerator, SWT.NONE);
		lblIdGeneratorType.setText("ID generator type:");

		cboIdGeneratorType = new DataComboViewer<>(groupIDGenerator, SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(IDGeneratorTypeEnumeration type) {
				return type.getName();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#onSelectionChanged(java.lang.Object)
			 */
			@Override
			public void onSelectionChanged(IDGeneratorTypeEnumeration type) {
				cboSequence.setEnabled(false);

				if (type == IDGeneratorTypeEnumeration.NONE || type == IDGeneratorTypeEnumeration.IDENTITY
						|| type == IDGeneratorTypeEnumeration.UUID) {
					txtInitialValue.setText("");
					txtBlockSize.setText("");
					txtInitialValue.setEnabled(false);
					txtBlockSize.setEnabled(false);

					domainObject.getIDGenerator().setBlockSize(0);
					domainObject.getIDGenerator().setInitialValue(0);
				}
				else {
					if (type == IDGeneratorTypeEnumeration.SEQUENCE)
						cboSequence.setEnabled(true);

					if (domainObject.getIDGenerator().getInitialValue() == 0) {
						domainObject.getIDGenerator().setInitialValue(DEFAULT_INIT_VALUE);
						txtInitialValue.setText(Integer.toString(DEFAULT_INIT_VALUE));
					}

					if (domainObject.getIDGenerator().getBlockSize() == 0) {
						domainObject.getIDGenerator().setBlockSize(DEFAULT_BLOCK_SIZE);
						txtBlockSize.setText(Integer.toString(DEFAULT_BLOCK_SIZE));
					}

					txtInitialValue.setEnabled(true);
					txtBlockSize.setEnabled(true);
				}
			}
		};

		cboIdGeneratorType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		cboIdGeneratorType.setData(idGenTypes);

		final var lblSequence = new Label(groupIDGenerator, SWT.NONE);
		lblSequence.setText("Sequence:");

		cboSequence = new DataComboViewer<>(groupIDGenerator, SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(String item) {
				return item;
			}
		};

		cboSequence.setData(sequences);
		cboSequence.setEnabled(false);
		cboSequence.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblInitialValue = new Label(groupIDGenerator, SWT.NONE);
		lblInitialValue.setText("Initial value:");

		txtInitialValue = new Text(groupIDGenerator, SWT.BORDER);
		txtInitialValue.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtInitialValue.setEnabled(false);

		final var lblBlockSize = new Label(groupIDGenerator, SWT.NONE);
		lblBlockSize.setText("Block size:");

		txtBlockSize = new Text(groupIDGenerator, SWT.BORDER);
		txtBlockSize.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
		txtBlockSize.setEnabled(false);

		txtDomainObjectName.setText(domainObject.getName());
		txtLabel.setText(domainObject.getLabel());
		txtNamePlural.setText(domainObject.getNamePlural());
		txtComment.setText(domainObject.getComment());
		txtLabelPlural.setText(domainObject.getLabelPlural());
		chkPropertyAccess.setSelection(domainObject.isPropertyAccess());
		txtInitialValue.setText(Integer.toString(domainObject.getIDGenerator().getInitialValue()));
		txtBlockSize.setText(Integer.toString(domainObject.getIDGenerator().getBlockSize()));
		cboIdGeneratorType.setSelectedItem(domainObject.getIDGenerator().getGeneratorType());

		if (domainObject.getDatabaseTable() != null)
			txtTableName.setText(domainObject.getDatabaseTable().getName());

		if (sequences.contains(domainObject.getIDGenerator().getName()))
			cboSequence.setSelectedItem(domainObject.getIDGenerator().getName());

		if (!editMode) {
			txtDomainObjectName.setEditable(false);
			txtDomainObjectName.setBackground(READ_ONLY_COLOR);
			txtNamePlural.setEditable(false);
			txtNamePlural.setBackground(READ_ONLY_COLOR);
			txtLabel.setEditable(false);
			txtLabel.setBackground(READ_ONLY_COLOR);
			txtLabelPlural.setEditable(false);
			txtLabelPlural.setBackground(READ_ONLY_COLOR);
			txtInitialValue.setEditable(false);
			txtInitialValue.setBackground(READ_ONLY_COLOR);
			txtBlockSize.setEditable(false);
			txtBlockSize.setBackground(READ_ONLY_COLOR);
			txtComment.setEditable(false);
			txtComment.setBackground(READ_ONLY_COLOR);
			chkPropertyAccess.setEnabled(false);
			cboIdGeneratorType.setEnabled(false);
			cboSequence.setEnabled(false);
		}

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		if (editMode)
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
		int blockSize = 0;
		int initialValue = 0;
		String msg = DomainModelValidationService.validateDomainObjectName(txtDomainObjectName.getText());

		if (msg != null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_INPUT_VAL, msg);
			return false;
		}

		msg = DomainModelValidationService.validateDomainObjectName(txtNamePlural.getText());

		if (msg != null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_INPUT_VAL, msg);
			return false;
		}

		msg = DomainModelValidationService.validateDomainObjectLabel(txtLabel.getText());

		if (msg != null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_INPUT_VAL, msg);
			return false;
		}

		msg = DomainModelValidationService.validateDomainObjectLabelPlural(txtLabelPlural.getText());

		if (msg != null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_INPUT_VAL, msg);
			return false;
		}

		if (txtBlockSize.isEnabled())
			try {
				if (!txtBlockSize.getText().isEmpty())
					blockSize = Integer.parseInt(txtBlockSize.getText());

				if (blockSize < 1)
					throw new NumberFormatException();
			}
			catch (final NumberFormatException ex) {
				MessageDialog.openInformation(getShell(), DLG_TITLE_INPUT_VAL, "The block size requires a positive integer value");
				return false;
			}

		if (txtInitialValue.isEnabled())
			try {
				if (!txtInitialValue.getText().isEmpty())
					initialValue = Integer.parseInt(txtInitialValue.getText());

				if (initialValue < 1)
					throw new NumberFormatException();
			}
			catch (final NumberFormatException ex) {
				MessageDialog.openInformation(getShell(), DLG_TITLE_INPUT_VAL, "The initial value requires a positive integer value");
				return false;
			}

		domainObject.setName(txtDomainObjectName.getText());
		domainObject.setNamePlural(txtNamePlural.getText());
		domainObject.setLabel(txtLabel.getText());
		domainObject.setLabelPlural(txtLabelPlural.getText());
		domainObject.setComment(txtComment.getText());
		domainObject.setPropertyAccess(chkPropertyAccess.getSelection());
		domainObject.getIDGenerator().setGeneratorType(cboIdGeneratorType.getSelectedItem());
		domainObject.getIDGenerator().setInitialValue(initialValue);
		domainObject.getIDGenerator().setBlockSize(blockSize);

		if (cboSequence.getSelectedItem() != null)
			domainObject.getIDGenerator().setName(cboSequence.getSelectedItem());

		return true;
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
