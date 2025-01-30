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
package net.codecadenza.eclipse.model.project.impl;

import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.BuildToolEnumeration;
import net.codecadenza.eclipse.model.project.ClientPlatformEnumeration;
import net.codecadenza.eclipse.model.project.Datasource;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.JPAVersionEnumeration;
import net.codecadenza.eclipse.model.project.MappingAnnotationStrategy;
import net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration;
import net.codecadenza.eclipse.model.project.PersistenceUnitProperty;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectFactory;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.project.ServerPlatformEnumeration;
import net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;
import net.codecadenza.eclipse.model.project.XMLMappingType;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * An implementation of the model factory.
 * @generated
 */
public class ProjectFactoryImpl extends EFactoryImpl implements ProjectFactory {
	/**
	 * @return the default factory implementation
	 * @generated
	 */
	public static ProjectFactory init() {
		try {
			final var theProjectFactory = (ProjectFactory) EPackage.Registry.INSTANCE.getEFactory(ProjectPackage.eNS_URI);

			if (theProjectFactory != null)
				return theProjectFactory;
		}
		catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}

		return new ProjectFactoryImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#create(org.eclipse.emf.ecore.EClass)
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		return switch (eClass.getClassifierID()) {
			case ProjectPackage.DATASOURCE -> createDatasource();
			case ProjectPackage.PERSISTENCE_UNIT_PROPERTY -> createPersistenceUnitProperty();
			case ProjectPackage.PROJECT -> createProject();
			case ProjectPackage.ROLE -> createRole();
			case ProjectPackage.BUILD_ARTIFACT -> createBuildArtifact();
			case ProjectPackage.INTEGRATION_MODULE -> createIntegrationModule();
			default -> throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#createFromString(org.eclipse.emf.ecore.EDataType, java.lang.String)
	 * @generated
	 */
	@Override
	public Object createFromString(EDataType eDataType, String initialValue) {
		return switch (eDataType.getClassifierID()) {
			case ProjectPackage.CLIENT_PLATFORM_ENUMERATION -> createClientPlatformEnumerationFromString(eDataType, initialValue);
			case ProjectPackage.SERVER_PLATFORM_ENUMERATION -> createServerPlatformEnumerationFromString(eDataType, initialValue);
			case ProjectPackage.PERSISTENCE_PROVIDER_ENUMERATION -> createPersistenceProviderEnumerationFromString(eDataType,
					initialValue);
			case ProjectPackage.JPA_VERSION_ENUMERATION -> createJPAVersionEnumerationFromString(eDataType, initialValue);
			case ProjectPackage.TECHNOLOGY_PLATFORM_ENUMERATION -> createTechnologyPlatformEnumerationFromString(eDataType,
					initialValue);
			case ProjectPackage.VALIDATION_TYPE_ENUMERATION -> createValidationTypeEnumerationFromString(eDataType, initialValue);
			case ProjectPackage.BUILD_TOOL_ENUMERATION -> createBuildToolEnumerationFromString(eDataType, initialValue);
			case ProjectPackage.BUILD_ARTIFACT_TYPE -> createBuildArtifactTypeFromString(eDataType, initialValue);
			case ProjectPackage.MAPPING_ANNOTATION_STRATEGY -> createMappingAnnotationStrategyFromString(eDataType, initialValue);
			case ProjectPackage.XML_MAPPING_TYPE -> createXMLMappingTypeFromString(eDataType, initialValue);
			case ProjectPackage.INTEGRATION_TECHNOLOGY -> createIntegrationTechnologyFromString(eDataType, initialValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#convertToString(org.eclipse.emf.ecore.EDataType, java.lang.Object)
	 * @generated
	 */
	@Override
	public String convertToString(EDataType eDataType, Object instanceValue) {
		return switch (eDataType.getClassifierID()) {
			case ProjectPackage.CLIENT_PLATFORM_ENUMERATION -> convertClientPlatformEnumerationToString(eDataType, instanceValue);
			case ProjectPackage.SERVER_PLATFORM_ENUMERATION -> convertServerPlatformEnumerationToString(eDataType, instanceValue);
			case ProjectPackage.PERSISTENCE_PROVIDER_ENUMERATION -> convertPersistenceProviderEnumerationToString(eDataType,
					instanceValue);
			case ProjectPackage.JPA_VERSION_ENUMERATION -> convertJPAVersionEnumerationToString(eDataType, instanceValue);
			case ProjectPackage.TECHNOLOGY_PLATFORM_ENUMERATION -> convertTechnologyPlatformEnumerationToString(eDataType,
					instanceValue);
			case ProjectPackage.VALIDATION_TYPE_ENUMERATION -> convertValidationTypeEnumerationToString(eDataType, instanceValue);
			case ProjectPackage.BUILD_TOOL_ENUMERATION -> convertBuildToolEnumerationToString(eDataType, instanceValue);
			case ProjectPackage.BUILD_ARTIFACT_TYPE -> convertBuildArtifactTypeToString(eDataType, instanceValue);
			case ProjectPackage.MAPPING_ANNOTATION_STRATEGY -> convertMappingAnnotationStrategyToString(eDataType, instanceValue);
			case ProjectPackage.XML_MAPPING_TYPE -> convertXMLMappingTypeToString(eDataType, instanceValue);
			case ProjectPackage.INTEGRATION_TECHNOLOGY -> convertIntegrationTechnologyToString(eDataType, instanceValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectFactory#createDatasource()
	 * @generated
	 */
	@Override
	public Datasource createDatasource() {
		return new DatasourceImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectFactory#createPersistenceUnitProperty()
	 * @generated
	 */
	@Override
	public PersistenceUnitProperty createPersistenceUnitProperty() {
		return new PersistenceUnitPropertyImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectFactory#createProject()
	 * @generated
	 */
	@Override
	public Project createProject() {
		return new ProjectImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectFactory#createRole()
	 * @generated
	 */
	@Override
	public Role createRole() {
		return new RoleImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectFactory#createBuildArtifact()
	 */
	@Override
	public BuildArtifact createBuildArtifact() {
		return new BuildArtifactImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectFactory#createIntegrationModule()
	 */
	@Override
	public IntegrationModule createIntegrationModule() {
		return new IntegrationModuleImpl();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public ClientPlatformEnumeration createClientPlatformEnumerationFromString(EDataType eDataType, String initialValue) {
		final ClientPlatformEnumeration result = ClientPlatformEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertClientPlatformEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public ServerPlatformEnumeration createServerPlatformEnumerationFromString(EDataType eDataType, String initialValue) {
		final ServerPlatformEnumeration result = ServerPlatformEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertServerPlatformEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public PersistenceProviderEnumeration createPersistenceProviderEnumerationFromString(EDataType eDataType, String initialValue) {
		final PersistenceProviderEnumeration result = PersistenceProviderEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertPersistenceProviderEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public JPAVersionEnumeration createJPAVersionEnumerationFromString(EDataType eDataType, String initialValue) {
		final JPAVersionEnumeration result = JPAVersionEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertJPAVersionEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public TechnologyPlatformEnumeration createTechnologyPlatformEnumerationFromString(EDataType eDataType, String initialValue) {
		final TechnologyPlatformEnumeration result = TechnologyPlatformEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertTechnologyPlatformEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public ValidationTypeEnumeration createValidationTypeEnumerationFromString(EDataType eDataType, String initialValue) {
		final ValidationTypeEnumeration result = ValidationTypeEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertValidationTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public BuildToolEnumeration createBuildToolEnumerationFromString(EDataType eDataType, String initialValue) {
		final BuildToolEnumeration result = BuildToolEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertBuildToolEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public BuildArtifactType createBuildArtifactTypeFromString(EDataType eDataType, String initialValue) {
		final BuildArtifactType result = BuildArtifactType.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertBuildArtifactTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public MappingAnnotationStrategy createMappingAnnotationStrategyFromString(EDataType eDataType, String initialValue) {
		final MappingAnnotationStrategy result = MappingAnnotationStrategy.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertMappingAnnotationStrategyToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public XMLMappingType createXMLMappingTypeFromString(EDataType eDataType, String initialValue) {
		final XMLMappingType result = XMLMappingType.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertXMLMappingTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public IntegrationTechnology createIntegrationTechnologyFromString(EDataType eDataType, String initialValue) {
		final IntegrationTechnology result = IntegrationTechnology.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertIntegrationTechnologyToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.ProjectFactory#getProjectPackage()
	 * @generated
	 */
	@Override
	public ProjectPackage getProjectPackage() {
		return (ProjectPackage) getEPackage();
	}

	/**
	 * @deprecated
	 * @return the project package
	 * @generated
	 */
	@Deprecated
	public static ProjectPackage getPackage() {
		return ProjectPackage.eINSTANCE;
	}

}
