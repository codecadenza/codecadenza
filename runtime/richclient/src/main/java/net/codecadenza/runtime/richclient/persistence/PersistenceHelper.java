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
package net.codecadenza.runtime.richclient.persistence;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Persistence helper to save client-related data
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PersistenceHelper {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static PersistentData p;
	private static String path;

	/**
	 * Constructor
	 */
	private PersistenceHelper() {

	}

	/**
	 * Initialize the persistence engine
	 * @param path
	 * @throws GeneralPersistenceExeption if the initialization has failed
	 */
	public static synchronized void initialize(String path) throws GeneralPersistenceExeption {
		final var file = new File(path);
		PersistenceHelper.path = path;

		if (file.exists()) {
			try (final var sin = new ObjectInputStream(new FileInputStream(file))) {
				p = (PersistentData) sin.readObject();
			}
			catch (final Exception e) {
				logger.error("Error while loading data from file {}!", path, e);

				throw new GeneralPersistenceExeption(e.getMessage());
			}
		}
		else
			p = new PersistentData();
	}

	/**
	 * Find persistent objects
	 * @param type
	 * @return a list of all objects of the given type
	 */
	@SuppressWarnings("unchecked")
	public static synchronized <T extends Serializable> List<T> findPersistentObjects(Class<T> type) {
		if (!p.getData().containsKey(type.getName()))
			p.getData().put(type.getName(), new ArrayList<>());

		return (List<T>) p.getData().get(type.getName());
	}

	/**
	 * Add a persistent object
	 * @param o
	 */
	public static synchronized void addPersistentObject(Serializable o) {
		List<Serializable> list = p.getData().get(o.getClass().getName());

		if (list == null) {
			list = new ArrayList<>();
			list.add(o);

			p.getData().put(o.getClass().getName(), list);

			return;
		}

		list.add(o);
	}

	/**
	 * Remove a persistent object
	 * @param o
	 */
	public static synchronized void removePersistentObject(Serializable o) {
		final List<Serializable> list = p.getData().get(o.getClass().getName());

		if (list != null)
			list.remove(o);
	}

	/**
	 * Save to persistent store
	 */
	public static synchronized void save() {
		logger.debug("Save property values");

		try (final var oos = new ObjectOutputStream(new FileOutputStream(path))) {
			oos.writeObject(p);
		}
		catch (final Exception e) {
			logger.error("Error while saving property values to file!", e);
		}
	}

}
