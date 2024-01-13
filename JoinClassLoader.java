// Copyright 2010 Christian d'Heureuse, Inventec Informatik AG, Zurich, Switzerland
// www.source-code.biz, www.inventec.ch/chdh
//
// This module is multi-licensed and may be used under the terms
// of any of the following licenses:
//
//  LGPL, GNU Lesser General Public License, V2 or later, http://www.gnu.org/licenses/lgpl.html
//  AL, Apache License, V2.0 or later, http://www.apache.org/licenses
//
// Please contact the author if you need another license.
// This module is provided "as is", without warranties of any kind.

package org.azf;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.ByteBuffer;
import java.security.SecureClassLoader;
import java.util.Enumeration;
import java.util.Vector;
import java.util.ArrayList;
import java.util.Collections;
import java.lang.Package;

/**
* A class loader that combines multiple class loaders into one.<br>
* The classes loaded by this class loader are associated with this class loader,
* i.e. Class.getClassLoader() points to this class loader.
*/
public class JoinClassLoader extends ClassLoader {

    public ClassLoader[] delegateClassLoaders;

    public JoinClassLoader (ClassLoader parent, ClassLoader... delegateClassLoadersParam) {
        super(parent);
        this.delegateClassLoaders = delegateClassLoadersParam;
    }

    public ClassLoader[] getDelegateClassLoaders() {
        return delegateClassLoaders;
    }

    public Package[] getGetPackages() {
        return this.getPackages();
    }

    @Override
    protected Package[] getPackages() {
        ArrayList<Package> result = new ArrayList<Package>();
        for (ClassLoader delegate : delegateClassLoaders) {
            Package[] delegatePackages = delegate.getDefinedPackages();
            for (Package delPackage: delegatePackages) {
                result.add(delPackage);
            }
        }
        Package[] packResult = new Package[result.size()];
        return result.toArray(packResult);
    }
}
