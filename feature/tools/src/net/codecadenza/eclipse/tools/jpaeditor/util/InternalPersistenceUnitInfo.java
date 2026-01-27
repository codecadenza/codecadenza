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
package net.codecadenza.eclipse.tools.jpaeditor.util;

import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.PersistenceUnit;
import jakarta.persistence.SharedCacheMode;
import jakarta.persistence.ValidationMode;
import jakarta.persistence.spi.ClassTransformer;
import jakarta.persistence.spi.PersistenceUnitInfo;
import jakarta.persistence.spi.PersistenceUnitTransactionType;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import javax.sql.DataSource;
import org.hibernate.jpa.HibernatePersistenceProvider;

/**
 * <p>
 * Implementation of a {@link PersistenceUnitInfo} for the JPA query editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class InternalPersistenceUnitInfo implements PersistenceUnitInfo {

	public static final String JPA_VERSION = "3.0";
	private static final String PERSISTENCE_UNIT_NAME = "jpa-editor";
	private final List<String> managedClassNames;
	private final Properties properties;
	private final List<ClassTransformer> transformers = new ArrayList<>();

	/**
	 * Constructor
	 * @param managedClassNames a list of all persistent classes that belong to the {@link PersistenceUnit}
	 * @param properties the properties for the configuration of the {@link EntityManagerFactory}
	 */
	public InternalPersistenceUnitInfo(List<String> managedClassNames, Properties properties) {
		this.managedClassNames = managedClassNames;
		this.properties = properties;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getPersistenceUnitName()
	 */
	@Override
	public String getPersistenceUnitName() {
		return PERSISTENCE_UNIT_NAME;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getPersistenceProviderClassName()
	 */
	@Override
	public String getPersistenceProviderClassName() {
		return HibernatePersistenceProvider.class.getName();
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getTransactionType()
	 */
	@SuppressWarnings("removal")
	@Override
	public PersistenceUnitTransactionType getTransactionType() {
		return PersistenceUnitTransactionType.RESOURCE_LOCAL;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getJtaDataSource()
	 */
	@Override
	public DataSource getJtaDataSource() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getNonJtaDataSource()
	 */
	@Override
	public DataSource getNonJtaDataSource() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getMappingFileNames()
	 */
	@Override
	public List<String> getMappingFileNames() {
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getJarFileUrls()
	 */
	@Override
	public List<URL> getJarFileUrls() {
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getPersistenceUnitRootUrl()
	 */
	@Override
	public URL getPersistenceUnitRootUrl() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getManagedClassNames()
	 */
	@Override
	public List<String> getManagedClassNames() {
		return managedClassNames;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#excludeUnlistedClasses()
	 */
	@Override
	public boolean excludeUnlistedClasses() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getSharedCacheMode()
	 */
	@Override
	public SharedCacheMode getSharedCacheMode() {
		return SharedCacheMode.NONE;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getValidationMode()
	 */
	@Override
	public ValidationMode getValidationMode() {
		return ValidationMode.AUTO;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getProperties()
	 */
	@Override
	public Properties getProperties() {
		return properties;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getPersistenceXMLSchemaVersion()
	 */
	@Override
	public String getPersistenceXMLSchemaVersion() {
		return JPA_VERSION;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getClassLoader()
	 */
	@Override
	public ClassLoader getClassLoader() {
		return Thread.currentThread().getContextClassLoader();
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#addTransformer(jakarta.persistence.spi.ClassTransformer)
	 */
	@Override
	public void addTransformer(ClassTransformer transformer) {
		transformers.add(transformer);
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getNewTempClassLoader()
	 */
	@Override
	public ClassLoader getNewTempClassLoader() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getQualifierAnnotationNames()
	 */
	@Override
	public List<String> getQualifierAnnotationNames() {
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.persistence.spi.PersistenceUnitInfo#getScopeAnnotationName()
	 */
	@Override
	public String getScopeAnnotationName() {
		return null;
	}

}
