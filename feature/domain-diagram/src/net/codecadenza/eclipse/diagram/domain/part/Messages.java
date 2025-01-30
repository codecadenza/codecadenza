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
package net.codecadenza.eclipse.diagram.domain.part;

import org.eclipse.osgi.util.NLS;

/**
 * <p>
 * Messages
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class Messages extends NLS {
	static {
		NLS.initializeMessages("messages", Messages.class);
	}

	/**
	 * Prevent instantiation
	 */
	private Messages() {
	}

	public static String CodeCadenzaCreationWizardTitle;
	public static String CodeCadenzaCreationWizard_DiagramModelFilePageTitle;
	public static String CodeCadenzaCreationWizard_DiagramModelFilePageDescription;
	public static String CodeCadenzaCreationWizard_DomainModelFilePageTitle;
	public static String CodeCadenzaCreationWizard_DomainModelFilePageDescription;
	public static String CodeCadenzaCreationWizardOpenEditorError;
	public static String CodeCadenzaCreationWizardCreationError;
	public static String CodeCadenzaCreationWizardPageExtensionError;
	public static String CodeCadenzaDiagramEditorUtil_OpenModelResourceErrorDialogTitle;
	public static String CodeCadenzaDiagramEditorUtil_OpenModelResourceErrorDialogMessage;
	public static String CodeCadenzaDiagramEditorUtil_CreateDiagramProgressTask;
	public static String CodeCadenzaDiagramEditorUtil_CreateDiagramCommandLabel;
	public static String CodeCadenzaDocumentProvider_isModifiable;
	public static String CodeCadenzaDocumentProvider_handleElementContentChanged;
	public static String CodeCadenzaDocumentProvider_IncorrectInputError;
	public static String CodeCadenzaDocumentProvider_NoDiagramInResourceError;
	public static String CodeCadenzaDocumentProvider_DiagramLoadingError;
	public static String CodeCadenzaDocumentProvider_UnsynchronizedFileSaveError;
	public static String CodeCadenzaDocumentProvider_SaveDiagramTask;
	public static String CodeCadenzaDocumentProvider_SaveNextResourceTask;
	public static String CodeCadenzaDocumentProvider_SaveAsOperation;
	public static String CodeCadenzaInitDiagramFileAction_InitDiagramFileResourceErrorDialogTitle;
	public static String CodeCadenzaInitDiagramFileAction_InitDiagramFileResourceErrorDialogMessage;
	public static String CodeCadenzaInitDiagramFileAction_InitDiagramFileWizardTitle;
	public static String CodeCadenzaInitDiagramFileAction_OpenModelFileDialogTitle;
	public static String CodeCadenzaNewDiagramFileWizard_CreationPageName;
	public static String CodeCadenzaNewDiagramFileWizard_CreationPageTitle;
	public static String CodeCadenzaNewDiagramFileWizard_CreationPageDescription;
	public static String CodeCadenzaNewDiagramFileWizard_RootSelectionPageName;
	public static String CodeCadenzaNewDiagramFileWizard_RootSelectionPageTitle;
	public static String CodeCadenzaNewDiagramFileWizard_RootSelectionPageDescription;
	public static String CodeCadenzaNewDiagramFileWizard_RootSelectionPageSelectionTitle;
	public static String CodeCadenzaNewDiagramFileWizard_RootSelectionPageNoSelectionMessage;
	public static String CodeCadenzaNewDiagramFileWizard_RootSelectionPageInvalidSelectionMessage;
	public static String CodeCadenzaNewDiagramFileWizard_InitDiagramCommand;
	public static String CodeCadenzaNewDiagramFileWizard_IncorrectRootError;
	public static String CodeCadenzaDiagramEditor_SavingDeletedFile;
	public static String CodeCadenzaDiagramEditor_SaveAsErrorTitle;
	public static String CodeCadenzaDiagramEditor_SaveAsErrorMessage;
	public static String CodeCadenzaDiagramEditor_SaveErrorTitle;
	public static String CodeCadenzaDiagramEditor_SaveErrorMessage;
	public static String CodeCadenzaElementChooserDialog_SelectModelElementTitle;
	public static String ModelElementSelectionPageMessage;
	public static String ValidateActionMessage;
	public static String Domainobjects1Group_title;
	public static String Domainobjects1Group_desc;
	public static String Associations2Group_title;
	public static String Associations2Group_desc;
	public static String DomainObject1CreationTool_title;
	public static String DomainObject1CreationTool_desc;
	public static String DomainAttribute2CreationTool_title;
	public static String DomainAttribute2CreationTool_desc;
	public static String DomainInheritance3CreationTool_title;
	public static String DomainInheritance3CreationTool_desc;
	public static String Enumeration4CreationTool_title;
	public static String Enumeration4CreationTool_desc;
	public static String EnumerationLiteral5CreationTool_title;
	public static String EnumerationLiteral5CreationTool_desc;
	public static String OneToOneAssociation1CreationTool_title;
	public static String OneToOneAssociation1CreationTool_desc;
	public static String OneToManyAssociation2CreationTool_title;
	public static String OneToManyAssociation2CreationTool_desc;
	public static String ManyToOneAssociation3CreationTool_title;
	public static String ManyToOneAssociation3CreationTool_desc;
	public static String ManyToManyAssociation4CreationTool_title;
	public static String ManyToManyAssociation4CreationTool_desc;
	public static String EnumAssociation5CreationTool_title;
	public static String EnumAssociation5CreationTool_desc;
	public static String DomainAttributeCompartmentEditPart_title;
	public static String JavaEnumEnumerationLiteralCompartmentEditPart_title;
	public static String CommandName_OpenDiagram;
	public static String NavigatorGroupName_DomainNamespace_1000_links;
	public static String NavigatorGroupName_DomainObject_2001_incominglinks;
	public static String NavigatorGroupName_DomainObject_2001_outgoinglinks;
	public static String NavigatorGroupName_JavaEnum_2002_incominglinks;
	public static String NavigatorGroupName_OneToOneAssociation_4003_target;
	public static String NavigatorGroupName_OneToOneAssociation_4003_source;
	public static String NavigatorGroupName_ManyToManyAssociation_4002_target;
	public static String NavigatorGroupName_ManyToManyAssociation_4002_source;
	public static String NavigatorGroupName_ManyToOneAssociation_4004_target;
	public static String NavigatorGroupName_ManyToOneAssociation_4004_source;
	public static String NavigatorGroupName_OneToManyAssociation_4006_target;
	public static String NavigatorGroupName_OneToManyAssociation_4006_source;
	public static String NavigatorGroupName_EnumAssociation_4001_target;
	public static String NavigatorGroupName_EnumAssociation_4001_source;
	public static String NavigatorGroupName_DomainInheritance_4005_target;
	public static String NavigatorGroupName_DomainInheritance_4005_source;
	public static String NavigatorActionProvider_OpenDiagramActionName;
	public static String AbstractParser_UnexpectedValueTypeMessage;
	public static String AbstractParser_WrongStringConversionMessage;
	public static String AbstractParser_UnknownLiteralMessage;
	public static String MessageFormatParser_InvalidInputError;
	public static String CodeCadenzaModelingAssistantProviderTitle;
	public static String CodeCadenzaModelingAssistantProviderMessage;

}
